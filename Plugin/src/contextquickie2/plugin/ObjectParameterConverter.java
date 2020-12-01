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

import java.util.HashMap;
import java.util.Map;
import org.eclipse.core.commands.AbstractParameterValueConverter;
import org.eclipse.core.commands.ParameterValueConversionException;

public class ObjectParameterConverter extends AbstractParameterValueConverter
{
  private static Map<String, Object> entries = new HashMap<String, Object>();

  @Override
  public Object convertToObject(String parameterValue) throws ParameterValueConversionException
  {
    if (entries.containsKey(parameterValue))
    {
      return entries.get(parameterValue);
    }
    return null;
  }

  @Override
  public String convertToString(Object parameterValue) throws ParameterValueConversionException
  {
    String hashCode = Integer.toString(parameterValue.hashCode());
    entries.put(hashCode, parameterValue);
    return hashCode;
  }

  public static void clearEntries()
  {
    entries.clear();
  }
}
