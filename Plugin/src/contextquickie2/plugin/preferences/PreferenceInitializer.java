package contextquickie2.plugin.preferences;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import contextquickie2.plugin.Activator;

public class PreferenceInitializer extends AbstractPreferenceInitializer
{
  private static final String PreferencesPrefix = "contextquickie2.plugin.preferences.";

  public static final String PreferenceNameShowWholeMenu = PreferencesPrefix + "ShowWholeMenu";

  public static final String PreferenceNameDipslayedMenus = PreferencesPrefix + "DisplayedMenus";

  /**
   * Configuration item for enabling/disabling showing the progress for external tools.
   */
  public static final String PreferenceNameShowProgressForExernalTools = 
      PreferencesPrefix + "ShowProgressForExternalTools";

  /**
   * Configuration item for enabling/disabling refresh of the workspace after execution of external applications.
   */
  public static final String PreferenceNameRefreshWorkspaceAfterExecution = 
      PreferencesPrefix + "RefreshWorkspaceAfterExecution";

  private static final Map<String, String> SupportedMenuExtensions = createSupportedMenuExtensionsMap();

  @Override
  public void initializeDefaultPreferences()
  {
    final IPreferenceStore store = Activator.getDefault().getPreferenceStore();
    store.setDefault(PreferenceNameShowWholeMenu, Boolean.TRUE.toString());
    store.setDefault(PreferenceNameDipslayedMenus, String.join(";", SupportedMenuExtensions.keySet()));
    store.setDefault(PreferenceNameShowProgressForExernalTools, true);
    store.setDefault(PreferenceNameRefreshWorkspaceAfterExecution, true);
  }
  
  /**
   * Gets a set containing all selected menu extensions.
   * @return A set containing all elected menu extensions.
   */
  public static Set<String> getSelectedMenuExtensions()
  {
    Set<String> selectedMenuExtensions = new HashSet<String>();
    final IPreferenceStore store = Activator.getDefault().getPreferenceStore();
    String selecttedExtensionsString = store.getString(PreferenceNameDipslayedMenus);
    for (String selectedExtension : selecttedExtensionsString.split(";"))
    {
      selectedMenuExtensions.add(selectedExtension);
    }
    
    return selectedMenuExtensions;
  }

  public static Set<String> getSupportedMenuExtensions()
  {
    return SupportedMenuExtensions.keySet();
  }
  
  public static String getMenuExtensionClassId(String extensionName)
  {
    return SupportedMenuExtensions.get(extensionName);
  }

  private static Map<String, String> createSupportedMenuExtensionsMap()
  {
    Map<String, String> entries = new HashMap<String, String>();
    entries.put("Beyond Compare 4", "{57FA2D12-D22D-490A-805A-5CB48E84F12A}");
    entries.put("Beyond Compare 5", "{812BC6B5-83CF-4AD9-97C1-6C60C8D025C5}");
    entries.put("Notepad++",        "{B298D29A-A6ED-11DE-BA8C-A68E55D89593}");
    entries.put("Open With",        "{09799AFB-AD67-11d1-ABCD-00C04FC30936}");
    entries.put("Path Copy Copy",   "{82CB99A2-2F18-4D5D-9476-54347E3B6720}");
    entries.put("TortoiseGit",      "{10A0FDD2-B0C0-4CD4-A7AE-E594CE3B91C8}");
    entries.put("TortoiseHg",       "{46605027-5B8C-4DCE-BFE0-051B7972D64C}");
    entries.put("TortoiseSVN",      "{30351349-7B7D-4FCC-81B4-1E394CA267EB}");
    entries.put("7-Zip",            "{23170F69-40C1-278A-1000-000100020000}");
    return entries;
  }
}
