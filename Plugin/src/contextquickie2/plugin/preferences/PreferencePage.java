package contextquickie2.plugin.preferences;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.RadioGroupFieldEditor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridData;
import org.eclipse.swt.layout.RowLayout;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.swt.widgets.Group;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import contextquickie2.plugin.Activator;

public class PreferencePage 
  extends FieldEditorPreferencePage 
  implements IWorkbenchPreferencePage, IPropertyChangeListener
{
  private RadioGroupFieldEditor radioGroupFieldEditor;

  private CheckedListBoxFieldEditor checkedListBoxFieldEditor;

  /**
   * Constructor.
   */
  public PreferencePage()
  {
    super(GRID);
    setPreferenceStore(Activator.getDefault().getPreferenceStore());
  }

  @Override
  public void init(IWorkbench workbench)
  {
  }

  @Override
  protected void createFieldEditors()
  {
    this.createApperanceFieldEditor();
    this.createEnabledContexMenusFileldEditor();
    this.createProgressHandlingFieldEditor();
  }

  @Override
  public final void propertyChange(final PropertyChangeEvent event)
  {
    if (event.getSource().equals(this.radioGroupFieldEditor))
    {
      this.updateBooleanEditorsEnabledState(event.getNewValue().toString());
    }
  }

  @Override
  protected void performDefaults()
  {
    super.performDefaults();
    this.updateBooleanEditorsEnabledState(
      this.getPreferenceStore().getString(PreferenceInitializer.PreferenceNameShowWholeMenu));
  }

  private void createApperanceFieldEditor()
  {
    this.radioGroupFieldEditor = new RadioGroupFieldEditor(
        PreferenceInitializer.PreferenceNameShowWholeMenu, 
        "Appearance", 
        1, 
        new String[][]
        {
          { "Show the whole context menu as submenu of the context menu", Boolean.TRUE.toString() },
          { "Show only selected items directly in the context menu", Boolean.FALSE.toString() },
        }, 
        this.getFieldEditorParent(), 
        true);
    this.addField(this.radioGroupFieldEditor);
  }

  private void createEnabledContexMenusFileldEditor()
  {
    List<String> extensions = new ArrayList<String>(PreferenceInitializer.getSupportedMenuExtensions());
    extensions.sort(Comparator.naturalOrder());
    this.checkedListBoxFieldEditor = new CheckedListBoxFieldEditor(
        PreferenceInitializer.PreferenceNameDipslayedMenus, 
        "Enabled Context Menus", 
        extensions.toArray(new String[extensions.size()]), 
        this.getFieldEditorParent());
    this.addField(this.checkedListBoxFieldEditor);

    this.updateBooleanEditorsEnabledState(
      this.getPreferenceStore().getString(PreferenceInitializer.PreferenceNameShowWholeMenu));
  }

  private void createProgressHandlingFieldEditor()
  {
    Group progressHandlingGroup = new Group(this.getFieldEditorParent(), SWT.NONE);
    progressHandlingGroup.setText("Progress Handling");
    progressHandlingGroup.setLayout(new RowLayout(SWT.VERTICAL));

    GridData gridData = new GridData();
    gridData.horizontalSpan = 2;
    gridData.horizontalAlignment = SWT.FILL;
    progressHandlingGroup.setLayoutData(gridData);

    Composite composite = new Composite(progressHandlingGroup, SWT.LEFT_TO_RIGHT);

    final BooleanFieldEditor showProgressForExternalTools = new BooleanFieldEditor(
        PreferenceInitializer.PreferenceNameShowProgressForExernalTools, 
        "Show progress when executing external tools",
        composite);
    this.addField(showProgressForExternalTools);

    final BooleanFieldEditor refreshWorkspaceEditor = new BooleanFieldEditor(
        PreferenceInitializer.PreferenceNameRefreshWorkspaceAfterExecution, 
        "Refresh workspace after executing external tools",
        composite);
    this.addField(refreshWorkspaceEditor);
  }

  private void updateBooleanEditorsEnabledState(String value)
  {
    boolean extensionsSelecable = true;
    if (value.equals(Boolean.TRUE.toString()))
    {
      extensionsSelecable = false;
    }

    this.checkedListBoxFieldEditor.setEnabled(extensionsSelecable, getFieldEditorParent());
  }
}
