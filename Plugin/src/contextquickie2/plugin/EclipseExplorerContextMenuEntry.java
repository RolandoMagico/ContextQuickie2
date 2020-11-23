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
import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.widgets.Display;

import explorercontextmenu.menu.ExplorerContextMenuEntry;

public class EclipseExplorerContextMenuEntry
{
  private Image nativeImage;
  
  private Image eclipseImage;
  
  private ImageDescriptor imageDescriptor;
  
  private ExplorerContextMenuEntry entry;
  
  private Set<IResource> selectedResources;
  
  private List<EclipseExplorerContextMenuEntry> entries = new ArrayList<EclipseExplorerContextMenuEntry>();
  
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
      this.entry.executeNativeCommand(false);
    }
  }
  
  public ImageDescriptor getImageDescriptor()
  {
    if ((this.imageDescriptor == null) && (this.entry.getImageHandle() != 0))
    {
      // The native image must be stored as class member and disposed during finalize
      // Disposing it already here will result in a crash during menu building
      this.nativeImage = Image.win32_new(Display.getCurrent(), SWT.BITMAP, this.entry.getImageHandle());
      ImageData imageData = this.nativeImage.getImageData();
      imageData.transparentPixel = 0;
      this.eclipseImage = new Image(Display.getCurrent(), imageData);
      this.imageDescriptor = ImageDescriptor.createFromImage(this.eclipseImage);
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
    if (this.nativeImage != null)
    {
      this.nativeImage.dispose();
    }

    if (this.eclipseImage != null)
    {
      this.eclipseImage.dispose();
    }
  }
}
