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

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IAdapterManager;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jface.action.IContributionItem;
import org.eclipse.jface.action.MenuManager;
import org.eclipse.jface.action.Separator;
import org.eclipse.jface.preference.IPreferenceStore;
import org.eclipse.jface.text.TextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.actions.CompoundContributionItem;
import org.eclipse.ui.menus.CommandContributionItem;
import org.eclipse.ui.menus.CommandContributionItemParameter;
import org.eclipse.ui.menus.IWorkbenchContribution;
import org.eclipse.ui.services.IServiceLocator;

import contextquickie2.plugin.preferences.PreferenceInitializer;
import explorercontextmenu.menu.ExplorerContextMenu;

public class MenuBuilder extends CompoundContributionItem implements IWorkbenchContribution
{
  private IServiceLocator serviceLocator;
  
  private EclipseExplorerContextMenuEntry contextMenu;
  
  public static final String ParameterExplorerContextMenu = "ContextQuickie2.Command.ExplorerContextMenuEntry";

  @Override
  public void initialize(IServiceLocator serviceLocator)
  {
    this.serviceLocator = serviceLocator;
  }

  @Override
  protected synchronized IContributionItem[] getContributionItems()
  {
    IContributionItem[] result = null;
    Set<IResource> selectedResources = this.getSelectedResources();

    // Get current preferences
    final IPreferenceStore store = Activator.getDefault().getPreferenceStore();
    String showAllString = store.getString(PreferenceInitializer.PreferenceNameShowWholeMenu);
    boolean showAll = Boolean.TRUE.toString().equals(showAllString);
    
    if (this.contextMenu == null)
    {
      ObjectParameterConverter.clearEntries();

      if (selectedResources.isEmpty() == false)
      {
        String[] paths = selectedResources.stream().map(resource -> this.convertIResourceToPath(resource))
            .toArray(String[]::new);
        String[] whitelistArray = null;
        
        if (Boolean.FALSE.toString().equals(showAllString))
        {
          List<String> whitelist = new ArrayList<String>();
          showAll = false;
          Set<String> displayedMenus = PreferenceInitializer.getSelectedMenuExtensions();

          for (String key : PreferenceInitializer.getSupportedMenuExtensions())
          {
            if (displayedMenus.contains(key))
            {
              whitelist.add(PreferenceInitializer.getMenuExtensionClassId(key));
            }
          }

          if (whitelist.isEmpty())
          {
            whitelist.add("DummyString");
          }

          whitelistArray = whitelist.toArray(new String[whitelist.size()]);
        }

        this.contextMenu = new EclipseExplorerContextMenuEntry(new ExplorerContextMenu(paths, showAll, whitelistArray));
        contextMenu.getWrappedEntry().setText("Explorer");
      }
    }

    if (this.contextMenu != null)
    {
      if (showAll == true)
      {
        IContributionItem menuRoot = this.createMenuEntry(this.contextMenu, selectedResources);
        if (menuRoot != null)
        {
          result = new IContributionItem[] { menuRoot };
        }
      }
      else
      {
        List<IContributionItem> contributionItems = new ArrayList<IContributionItem>();
        for (EclipseExplorerContextMenuEntry entry : this.contextMenu.getEntries())
        {
          contributionItems.add(this.createMenuEntry(entry, selectedResources));
        }
        
        result = contributionItems.toArray(new IContributionItem[contributionItems.size()]);
      }
      this.setVisible(true);
    }
    else
    {
      result = new IContributionItem[0];
      this.setVisible(false);
    }
    
    return result;
  }

  private IContributionItem createMenuEntry(EclipseExplorerContextMenuEntry entry, Set<IResource> selectedResources)
  {
    IContributionItem result = null;
    
    entry.setSelectedResources(selectedResources);

    if (entry.getWrappedEntry().isSeperator())
    {
      result = new Separator();
    }
    else if (entry.getEntries().iterator().hasNext())
    {
      final MenuManager subMenu = new MenuManager(entry.getWrappedEntry().getText(), null, null);
      Iterator<EclipseExplorerContextMenuEntry> iterator = entry.getEntries().iterator();
      subMenu.setImageDescriptor(entry.getImageDescriptor());
      while (iterator.hasNext())
      {
        subMenu.add(this.createMenuEntry(iterator.next(), selectedResources));
      }
      
      result = subMenu;
    }
    else
    {
      final CommandContributionItemParameter commandParameter = new CommandContributionItemParameter(
        this.serviceLocator, 
        null,
        "ContextQuickie2.Command", 
        CommandContributionItem.STYLE_PUSH);

      // Create map of parameters for the command
      final Map<String, Object> parameters = new HashMap<String, Object>();
      parameters.put(ParameterExplorerContextMenu, entry);
      commandParameter.parameters = parameters;
      commandParameter.label = entry.getWrappedEntry().getText();
      commandParameter.tooltip = entry.getWrappedEntry().getHelpText();
      commandParameter.icon = entry.getImageDescriptor();
      result = new CommandContributionItem(commandParameter);
    }

    return result;
  }
  
  private Set<IResource> getSelectedResources()
  {
    final Set<IResource> selectedResources = new HashSet<IResource>();

    final IWorkbenchWindow window = PlatformUI.getWorkbench().getActiveWorkbenchWindow();
    if (window != null)
    {
      final ISelection selection = window.getSelectionService().getSelection();
      if (selection != null)
      {
        final IAdapterManager adapterManager = Platform.getAdapterManager();

        if ((selection instanceof ITreeSelection) && (selection.isEmpty() == false))
        {
          // Context menu has been opened in a tree view
          for (Object selectedItem : ((IStructuredSelection) selection).toList())
          {
            final IResource resource = adapterManager.getAdapter(selectedItem, IResource.class);
            if (this.convertIResourceToPath(resource) != null)
            {
              selectedResources.add(resource);
            }
          }
        }
        else if (selection instanceof TextSelection)
        {
          // Context menu has been opened in an editor
          IEditorPart editor = window.getActivePage().getActiveEditor();
          if (editor != null)
          {
            final IResource resource = adapterManager.getAdapter(editor.getEditorInput(), IResource.class);
            if (this.convertIResourceToPath(resource) != null)
            {
              selectedResources.add(resource);
            }
          }
        }
      }
    }

    return selectedResources;
  }
  
  private String convertIResourceToPath(IResource resource)
  {
    String path = null;
    if (resource != null)
    {
      IPath location = resource.getLocation();
      if (location != null)
      {
        path = location.toOSString();
      }
    }
    
    return path;
  }
}
