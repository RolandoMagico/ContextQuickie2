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
package explorercontextmenu.menu;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.resource.ImageDescriptor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.graphics.Image;
import org.eclipse.swt.graphics.ImageData;
import org.eclipse.swt.widgets.Display;

public class ExplorerContextMenuEntry
{
  private List<ExplorerContextMenuEntry> entries = new ArrayList<ExplorerContextMenuEntry>();
  
  private String text;

  private String helpText;
  
  private int commandId;
  
  private boolean isSeperator;
  
  private Image nativeImage;
  
  private Image eclipseImage;
  
  private ImageDescriptor imageDescriptor;
  
  private long nativeHandle;
  
  public Iterable<ExplorerContextMenuEntry> getEntries()
  {
    return this.entries;
  }
  
  public void addEntry(ExplorerContextMenuEntry value) 
  {
    this.entries.add(value);
  }

  public String getText()
  {
    return this.text;
  }

  public void setText(String value)
  {
    this.text = value;
  }
  
  public String getHelpText()
  {
    return helpText;
  }

  public void setHelpText(String value)
  {
    this.helpText = value;
  }

  public int getCommandId()
  {
    return this.commandId;
  }

  public void setCommandId(int value)
  {
    this.commandId = value;
  }

  public boolean isSeperator()
  {
    return this.isSeperator;
  }

  public void setSeperator(boolean value)
  {
    this.isSeperator = value;
  }

  public void setImageHandle(long value)
  {
    if (value != 0)
    {
      this.nativeImage = Image.win32_new(Display.getCurrent(), SWT.BITMAP, value);
      ImageData imageData = this.nativeImage.getImageData();
      imageData.transparentPixel = 0;
      this.eclipseImage = new Image(Display.getCurrent(), imageData);
      this.imageDescriptor = ImageDescriptor.createFromImage(this.eclipseImage);
    }
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

  public long getNativeHandle()
  {
    return this.nativeHandle;
  }

  public void setNativeHandle(long value)
  {
    this.nativeHandle = value;
  }
  
  public ImageDescriptor getImageDescriptor()
  {
    return this.imageDescriptor;
  }

  public native void executeCommand();
}
