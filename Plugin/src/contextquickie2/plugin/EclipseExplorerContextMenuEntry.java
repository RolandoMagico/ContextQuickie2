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
import java.util.List;
import java.util.Set;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.jobs.Job;
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.widgets.Display;

import explorercontextmenu.menu.ExplorerContextMenuEntry;
import explorercontextmenu.menu.ProcessInfo;
import explorercontextmenu.menu.ProcessMonitor;

public class EclipseExplorerContextMenuEntry
{
  private Image eclipseImage;
  
  private ImageDescriptor imageDescriptor;
  
  private ExplorerContextMenuEntry entry;
  
  private Set<IResource> selectedResources;
  
  private List<EclipseExplorerContextMenuEntry> entries = new ArrayList<EclipseExplorerContextMenuEntry>();
  
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
          e.printStackTrace();
        }
      }
    }
    else
    {
      ProcessMonitor processMonitor = new ProcessMonitor();
      processMonitor.createFirstSnapshot();
      this.entry.executeNativeCommand(false);
      processMonitor.createSecondSnapshot();

      ProcessInfo createdProcess = processMonitor.getCreatedProcess();
      if (createdProcess != null)
      {
        new Thread(() -> this.runMonitorJobs(createdProcess)).start();
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

        this.eclipseImage = new Image(Display.getCurrent(), imageData);
        this.imageDescriptor = ImageDescriptor.createFromImage(this.eclipseImage);
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
 
  @Override
  protected void finalize() throws Throwable
  {
    if (this.eclipseImage != null)
    {
      this.eclipseImage.dispose();
    }
  }

  private void runMonitorJobs(ProcessInfo processInfo)
  {
    final String progresstitle = Activator.PLUGIN_ID;
    Job job = null;

    {
      job = new Job(progresstitle) 
      {
        protected IStatus run(IProgressMonitor monitor)
        {
          IStatus status = EclipseExplorerContextMenuEntry.this.waitForProcessToFinish(processInfo, monitor);
          
          if (status == Status.OK_STATUS)
          {
            status = EclipseExplorerContextMenuEntry.this.refreshResourcesAfterProcessEnd(monitor);
          }
          
          return status;
        }
      };
    }
    
    if (job != null)
    {
      job.schedule();
    }
  }
  
  private IStatus waitForProcessToFinish(ProcessInfo processInfo, IProgressMonitor monitor)
  {
    if (processInfo != null)
    {
      if (monitor != null)
      {
        monitor.setTaskName("Running external application");
      }
      ProcessMonitor processMonitor = new ProcessMonitor();
      while (processMonitor.isProcessRunning(processInfo))
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
          e.printStackTrace();
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
