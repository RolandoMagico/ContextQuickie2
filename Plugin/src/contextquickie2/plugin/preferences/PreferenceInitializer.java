package contextquickie2.plugin.preferences;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.runtime.preferences.AbstractPreferenceInitializer;
import org.eclipse.jface.preference.IPreferenceStore;

import contextquickie2.plugin.Activator;

public class PreferenceInitializer extends AbstractPreferenceInitializer
{
  private static final String PreferencesPrefix = "contextquickie2.plugin.preferences.";

  public static final String PreferenceNameShowWholeMenu = PreferencesPrefix + "ShowWholeMenu";

  public static final String PreferenceNameExtensionPrefix = PreferencesPrefix + "Show.";

  public static final Map<String, String> SupportedMenuExtensions = createSupportedMenuExtensionsMap();

  @Override
  public void initializeDefaultPreferences()
  {
    final IPreferenceStore store = Activator.getDefault().getPreferenceStore();
    store.setDefault(PreferenceNameShowWholeMenu, Boolean.TRUE.toString());

    for (String key : SupportedMenuExtensions.keySet())
    {
      store.setDefault(PreferenceNameExtensionPrefix + key, true);
    }
  }

  private static Map<String, String> createSupportedMenuExtensionsMap()
  {
    Map<String, String> entries = new HashMap<String, String>();
    entries.put("Beyond Compare", "{57FA2D12-D22D-490A-805A-5CB48E84F12A}");
    entries.put("Notepad++",      "{B298D29A-A6ED-11DE-BA8C-A68E55D89593}");
    entries.put("Open With",      "{09799AFB-AD67-11d1-ABCD-00C04FC30936}");
    entries.put("TortoiseGit",    "{10A0FDD2-B0C0-4CD4-A7AE-E594CE3B91C8}");
    entries.put("TortoiseSVN",    "{30351349-7B7D-4FCC-81B4-1E394CA267EB}");
    return entries;
  }
}