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
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.graphics.PaletteData;
import org.eclipse.swt.widgets.Display;

import explorercontextmenu.menu.ExplorerContextMenuEntry;

public class EclipseExplorerContextMenuEntry
{
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
    if (this.imageDescriptor == null)
    {
      int depth = this.entry.getImageDepth();
      int height = this.entry.getImageHeigth();
      int width = this.entry.getImageWidth();
      byte[] data = this.entry.getImageData();
      if ((depth != 0) && (height != 0) &&( width != 0) && (data != null))
      {
        ImageData imageData = new ImageData(width, height, 32, new PaletteData(0xFF00,0xFF0000,0xFF000000), 4, data);
        imageData.data = data;
        imageData.alphaData = new byte[width*height];
        for (int i = 0; i < imageData.alphaData.length; i++)
        {
          imageData.alphaData[i] = data[i * 4 + 3];
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
}
