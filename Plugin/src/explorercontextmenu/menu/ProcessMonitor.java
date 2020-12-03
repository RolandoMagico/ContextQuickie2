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
import java.util.Arrays;
import java.util.List;

public class ProcessMonitor
{
  private List<ProcessInfo> firstSnapshot;
  
  private List<ProcessInfo> secondSnapshot;
  
  public void createFirstSnapshot()
  {
    this.firstSnapshot = Arrays.asList(this.getCurrentChildProcesses());
  }
  
  public void createSecondSnapshot()
  {
    this.secondSnapshot = Arrays.asList(this.getCurrentChildProcesses());
  }
  
  public boolean isProcessRunning(ProcessInfo processInfo)
  {
    return Arrays.asList(this.getCurrentChildProcesses()).contains(processInfo);
  }
  
  /**
   * Checks if there is exactly one child process created between the first and the second snapshot.
   * If there is one process and the process is a known process, the corresponding ProcessInfo is returned.
   * @return The ProcessInfo for the created process or null if there is no new process or the created process
   *     is unknown.
   */
  public ProcessInfo getCreatedProcess()
  {
    ProcessInfo result = null;
    if ((this.firstSnapshot != null) && (this.secondSnapshot != null))
    {
      List<ProcessInfo> secondSnapShotCopy = new ArrayList<ProcessInfo>(this.secondSnapshot);
      secondSnapShotCopy.removeAll(this.firstSnapshot);

      if (secondSnapShotCopy.size() == 1)
      {
        result = secondSnapShotCopy.get(0);
      }
    }
    
    return result;
  }

  private native ProcessInfo[] getCurrentChildProcesses();
}
