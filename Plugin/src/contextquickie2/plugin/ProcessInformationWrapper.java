package contextquickie2.plugin;

import java.util.ArrayList;
import java.util.List;

import rolandomagico.processinformation.ProcessData;
import rolandomagico.processinformation.ProcessInformation;

public class ProcessInformationWrapper extends ProcessInformation
{
  private List<ProcessData> firstSnapshot;
  
  private List<ProcessData> secondSnapshot;
  
  public void createFirstSnapshot()
  {
    this.firstSnapshot = getCurrentChildProcesses();
  }
  
  public void createSecondSnapshot()
  {
    this.secondSnapshot = getCurrentChildProcesses();
  }
  
  /**
   * Checks if there is exactly one child process created between the first and the second snapshot.
   * If there is one process and the process is a known process, the corresponding ProcessData is returned.
   * @return The ProcessData for the created process or null if there is no new process or the created process
   *     is unknown.
   */
  public ProcessData getCreatedProcess()
  {
    ProcessData result = null;
    if ((this.firstSnapshot != null) && (this.secondSnapshot != null))
    {
      List<ProcessData> secondSnapShotCopy = new ArrayList<ProcessData>(this.secondSnapshot);
      secondSnapShotCopy.removeAll(this.firstSnapshot);

      if (secondSnapShotCopy.size() == 1)
      {
        result = secondSnapShotCopy.get(0);
      }
    }
    
    return result;
  }
  
  public static boolean isProcessRunning(ProcessData processData)
  {
    return getCurrentChildProcesses().contains(processData);
  }
  
  /**
   * Gets all child processes of the current process.
   * @return
   *     A list with all child processes of the current process.
   */
  public static List<ProcessData> getCurrentChildProcesses()
  {
    long currentProcessId = getCurrentProcessId();
    List<ProcessData> processes = getAllProcesses();
    processes.removeIf(process -> process.parantProcessId != currentProcessId);
    return processes;
  }
}
