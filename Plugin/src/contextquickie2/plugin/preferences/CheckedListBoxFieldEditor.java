package contextquickie2.plugin.preferences;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.eclipse.jface.preference.FieldEditor;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;

public class CheckedListBoxFieldEditor extends FieldEditor
{
  private List<Button> buttons;
  
  private String[] buttonLabels;

  /**
   * Constructor.
   * @param name
   *      The name of the preference.
   * @param labelText
   *      The text which is shown as group text.
   * @param values
   *      A list of values which can be selected.
   * @param parent
   *      The parent element.
   */
  public CheckedListBoxFieldEditor(String name, String labelText, String[] values, Composite parent)
  {
    this.init(name, labelText);
    this.buttons = new ArrayList<Button>();
    this.buttonLabels = values;
    this.createControl(parent);
  }
  
  @Override
  protected void adjustForNumColumns(int numColumns)
  {
  }

  @Override
  protected void doFillIntoGrid(Composite parent, int numColumns)
  {
    Group mainGroup = new Group(parent, SWT.NONE);
    mainGroup.setText(this.getLabelText());
    mainGroup.setLayout(new RowLayout(SWT.VERTICAL));
    
    GridData gridData = new GridData();
    gridData.horizontalSpan = numColumns;
    gridData.horizontalAlignment = SWT.FILL;
    mainGroup.setLayoutData(gridData);

    this.buttons.clear();
    for (String value : this.buttonLabels)
    {
      Button button = new Button(mainGroup, SWT.CHECK);
      button.setText(value);
      button.setData(this.getPreferenceName() + value);
      this.buttons.add(button);
    }
  }

  @Override
  protected void doLoad()
  {
    Set<String> selectedMenuExtensions = PreferenceInitializer.getSelectedMenuExtensions();
    for (Button button : this.buttons)
    {
      button.setSelection(selectedMenuExtensions.contains(button.getText()));
    }
  }

  @Override
  protected void doLoadDefault()
  {
    for (Button button : this.buttons)
    {
      button.setSelection(true);
    }
  }

  @Override
  protected void doStore()
  {
    Set<String> selectedMenuExtensions = new HashSet<String>();
    for (Button button : this.buttons)
    {
      if (button.getSelection() == true)
      {
        selectedMenuExtensions.add(button.getText());
      }
    }
    
    this.getPreferenceStore().setValue(this.getPreferenceName(), String.join(";", selectedMenuExtensions));
  }

  @Override
  public int getNumberOfControls()
  {
    return 1;
  }

  @Override
  public void setEnabled(boolean enabled, Composite parent)
  {
    for (Button button : this.buttons)
    {
      button.setEnabled(enabled);
    }
  }

}
