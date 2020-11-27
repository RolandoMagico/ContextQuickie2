package contextquickie2.plugin.preferences;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import org.eclipse.jface.preference.BooleanFieldEditor;
import org.eclipse.jface.preference.FieldEditorPreferencePage;
import org.eclipse.jface.preference.RadioGroupFieldEditor;
import org.eclipse.jface.util.IPropertyChangeListener;
import org.eclipse.jface.util.PropertyChangeEvent;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPreferencePage;

import contextquickie2.plugin.Activator;

public class PreferencePage extends FieldEditorPreferencePage implements IWorkbenchPreferencePage, IPropertyChangeListener
{
  private RadioGroupFieldEditor radioGroupFieldEditor;

  private List<BooleanFieldEditor> booleanFieldEditors = new ArrayList<BooleanFieldEditor>();

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


    List<String> extensions = new ArrayList<String>(PreferenceInitializer.SupportedMenuExtensions.keySet());
    extensions.sort(Comparator.naturalOrder());
    for (String key : extensions)
    {
      BooleanFieldEditor editor = new BooleanFieldEditor(
          PreferenceInitializer.PreferenceNameExtensionPrefix + key,
          "Show " + key,
          getFieldEditorParent());
      this.booleanFieldEditors.add(editor);
      this.addField(editor);
    }

    this.updateBooleanEditorsEnabledState(this.getPreferenceStore().getString(PreferenceInitializer.PreferenceNameShowWholeMenu));
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
    this.updateBooleanEditorsEnabledState(this.getPreferenceStore().getString(PreferenceInitializer.PreferenceNameShowWholeMenu));
  }

  private void updateBooleanEditorsEnabledState(String value)
  {
    boolean extensionsSelecable = true;
    if (value.equals(Boolean.TRUE.toString()))
    {
      extensionsSelecable = false;
    }

    for (BooleanFieldEditor editor : this.booleanFieldEditors)
    {
      editor.setEnabled(extensionsSelecable, getFieldEditorParent());
    }
  }
}