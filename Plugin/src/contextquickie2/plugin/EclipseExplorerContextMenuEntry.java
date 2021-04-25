/***********************************************************************************************************************
 MIT License

 Copyright(c) 2020 Roland Reinl

 Permission is hereby granted, free of charge, to any person obtaining a copy
 of this software and associated documentation files(the "Software"), to deal
 in the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and /or sell
 copies of the Software, and to permit persons to whom the Software is
 furnished to do so, subject to the following conditions :

 The above copyright notice and this permission notice shall be included in all
 copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.IN NO EVENT SHALL THE
 AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 SOFTWARE.
***********************************************************************************************************************/

package contextquickie2.plugin;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.widgets.Display;
import org.eclipse.ui.PlatformUI;
import org.osgi.framework.FrameworkUtil;

import contextquickie2.plugin.preferences.PreferenceInitializer;
import explorercontextmenu.menu.ExplorerContextMenuEntry;
import rolandomagico.processinformation.ProcessData;

public class EclipseExplorerContextMenuEntry
{
  /**
   * A list with process names for which updating the selected resources after command execution is supported. 
   */
  private static List<String> supportedProcesses = Arrays.asList(
      "BCompare.exe",         // Beyond Compare
      "TortoiseGitProc.exe",  // TortoiseGit
      "thgw.exe",             // TortoiseHg
      "TortoiseProc.exe"      // TortoiseSVN
      );

  private Image image;
  
  private ImageDescriptor imageDescriptor;
  
  private ExplorerContextMenuEntry entry;
  
  private Set<IResource> selectedResources;
  
  private List<EclipseExplorerContextMenuEntry> entries = new ArrayList<EclipseExplorerContextMenuEntry>();
  
  private final ILog logger = Platform.getLog(FrameworkUtil.getBundle(this.getClass()));

  /**
   * Constructor.
   * 
   * @param wrappedEntry
   *      The entry which is wrapped by this instance.
   */
  public EclipseExplorerContextMenuEntry(ExplorerContextMenuEntry wrappedEntry)
  {
    this.entry = wrappedEntry;
    
    for (ExplorerContextMenuEntry entry : this.entry.getEntries())
    {
      entries.add(new EclipseExplorerContextMenuEntry(entry));
    }
  }

  public Iterable<EclipseExplorerContextMenuEntry> getEntries()
  {
    return this.entries;
  }
  
  /**
   * Executes the command of this entry.
   */
  public void executeCommand()
  {
    if ((this.entry.getCommandString() != null) && (this.entry.getCommandString().equals("open")))
    {
      for (IResource resource : this.getSelectedResources())
      {
        try
        {
          Runtime.getRuntime().exec(new String [] { "explorer.exe", resource.getLocation().toOSString() });
        }
        catch (IOException e)
        {
          logger.log(new Status(IStatus.ERROR, Activator.PLUGIN_ID, e.getMessage(), e));
        }
      }
    }
    else
    {
      ProcessInformationWrapper processMonitor = new ProcessInformationWrapper();
      processMonitor.createFirstSnapshot();
      long windowHandle = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell().handle;
      int result = this.entry.executeCommand(windowHandle, false);
      if (result == 0)
      {
        processMonitor.createSecondSnapshot();

        ProcessData createdProcess = processMonitor.getCreatedProcess();
        if ((createdProcess != null) && (supportedProcesses.contains(createdProcess.name)))
        {
          new Thread(() -> this.runMonitorJobs(createdProcess)).start();
        }
      }
      else
      {
        final ILog logger = Platform.getLog(FrameworkUtil.getBundle(EclipseExplorerContextMenuEntry.class));
        String message = String.format(
            "Unable to execute context menu command. Entry: %s, Command ID: %d, Command String: %s. Error Code: 0x%x",
            this.entry.getText(),
            this.entry.getCommandId(),
            this.entry.getCommandString(),
            result);
        logger.log(new Status(IStatus.ERROR, Activator.PLUGIN_ID, message));
      }
    }
  }

  
  /**
   * Gets the image descriptor of this instance.
   * @return The image descriptor of this instance or null if no image is available.
   */
  public ImageDescriptor getImageDescriptor()
  {
    if (this.imageDescriptor == null)
    {
      int depth = this.entry.getImageDepth();
      int height = this.entry.getImageHeigth();
      int width = this.entry.getImageWidth();
      byte[] data = this.entry.getImageData();
      if ((depth != 0) && (height != 0) && (width != 0) && (data != null))
      {
        ImageData imageData = new ImageData(width, height, 32, new PaletteData(0xFF00, 0xFF0000, 0xFF000000), 4, data);
        imageData.data = data;
        imageData.alphaData = new byte[width * height];
        for (int i = 0; i < imageData.alphaData.length; i++)
        {
          imageData.alphaData[i] = (byte) (data[i * 4] | data[i * 4  + 1] | data[i * 4  + 2] | data[i * 4 + 3]);
        }

        this.image = new Image(Display.getCurrent(), imageData);
        this.imageDescriptor = ImageDescriptor.createFromImage(this.image);
      }
    }

    return this.imageDescriptor;
  }
  
  public Set<IResource> getSelectedResources()
  {
    return this.selectedResources;
  }

  public void setSelectedResources(Set<IResource> value)
  {
    this.selectedResources = value;
  }
  
  public ExplorerContextMenuEntry getWrappedEntry()
  {
    return this.entry;
  }
  
  /**
   * Releases all unmanaged resources (images) which are used by this instance. 
   */
  public void dispose()
  {
    if ((this.image != null) && (this.image.isDisposed() == false))
    {
      this.image.dispose();
      this.imageDescriptor = null;
    }
  }

  private void runMonitorJobs(ProcessData processInfo)
  {
    final String progresstitle = Activator.PLUGIN_ID;
    final IPreferenceStore preferenceStore = Activator.getDefault().getPreferenceStore();
    final boolean showProgressForExternalTools = preferenceStore.getBoolean(
        PreferenceInitializer.PreferenceNameShowProgressForExernalTools);
    final boolean refreshWorkspaceAfterExecution = preferenceStore.getBoolean(
        PreferenceInitializer.PreferenceNameRefreshWorkspaceAfterExecution);

    if ((showProgressForExternalTools == true) || (refreshWorkspaceAfterExecution == true))
    {
      Job job = new Job(progresstitle) 
      {
        protected IStatus run(IProgressMonitor monitor)
        {
          IStatus status = Status.OK_STATUS;

          if (showProgressForExternalTools == true)
          {
            status = EclipseExplorerContextMenuEntry.this.waitForProcessToFinish(processInfo, monitor);
          }
          else if (refreshWorkspaceAfterExecution == true)
          {
            // showProgressForExternalTools is set to false, otherwise the previous if condition would have been met
            // refreshWorkspaceAfterExecution is set to true, so wait for the process to finish without monitoring.
            // Without monitoring, no progress is reported in Eclipse. But it is needed anyway to refresh the workspace.
            EclipseExplorerContextMenuEntry.this.waitForProcessToFinish(processInfo, null);
          }
          
          if ((status == Status.OK_STATUS) && (refreshWorkspaceAfterExecution == true))
          {
            status = EclipseExplorerContextMenuEntry.this.refreshResourcesAfterProcessEnd(monitor);
          }
          
          return status;
        }
      };

      job.schedule();
    }
  }
  
  private IStatus waitForProcessToFinish(ProcessData processInfo, IProgressMonitor monitor)
  {
    if (processInfo != null)
    {
      if (monitor != null)
      {
        monitor.setTaskName("Running external application");
      }

      while (ProcessInformationWrapper.isProcessRunning(processInfo))
      {
        if ((monitor != null) && (monitor.isCanceled()))
        {
          return Status.CANCEL_STATUS;
        }
        try
        {
          Thread.sleep(1000);
        }
        catch (InterruptedException e)
        {
          logger.log(new Status(IStatus.ERROR, Activator.PLUGIN_ID, e.getMessage(), e));
          return Status.CANCEL_STATUS;
        }
      }
    }
    
    return Status.OK_STATUS;
  }

  private IStatus refreshResourcesAfterProcessEnd(IProgressMonitor monitor)
  {
    {
      monitor.setTaskName("Refreshing workspace");
      for (IResource resource : this.getSelectedResources())
      {
        if (monitor.isCanceled())
        {
          return Status.CANCEL_STATUS;
        }
        if ((resource.getParent() != null) && (resource.getType() != IResource.PROJECT))
        {
          resource = resource.getParent();
        }
        try
        {
          resource.refreshLocal(IResource.DEPTH_INFINITE, monitor);
        }
        catch (CoreException e)
        {
          e.printStackTrace();
          return Status.CANCEL_STATUS;
        }
      }
    }
    
    return Status.OK_STATUS;
  }
}
