program ankat;

uses
  Vcl.Forms,
  Vcl.Controls,
  Unit1 in 'Unit1.pas' {Form1},
  UnitFormNewPartyDialog in 'UnitFormNewPartyDialog.pas' {FormNewPartyDialog},
  pipe in 'utils\pipe.pas',
  richeditutils in 'utils\richeditutils.pas',
  stringgridutils in 'utils\stringgridutils.pas',
  listports in 'utils\listports.pas',
  UnitFrameSettings in 'UnitFrameSettings.pas' {FrameSettings: TFrame},
  stringutils in 'utils\stringutils.pas',
  UnitData in 'UnitData.pas' {DataModule1: TDataModule},
  UnitFrameDelay in 'UnitFrameDelay.pas' {FrameDelay: TFrame},
  UnitFormLogNodes in 'UnitFormLogNodes.pas',
  CurrentWorkTreeData in 'CurrentWorkTreeData.pas',
  settings in 'settings.pas',
  UnitFormPopup in 'UnitFormPopup.pas' {FormPopup},
  UnitFormLog in 'UnitFormLog.pas' {FormLog},
  UnitFrameCoef in 'UnitFrameCoef.pas' {FrameCoef: TFrame},
  UnitFrameVar in 'UnitFrameVar.pas' {FrameVar: TFrame},
  UnitFormManualControl in 'UnitFormManualControl.pas' {FormManualControl},
  UnitFormChart in 'UnitFormChart.pas' {FormChart},
  UnitFormChartNodes in 'UnitFormChartNodes.pas',
  msglevel in 'msglevel.pas',
  parties in 'parties.pas',
  variantutils in 'utils\variantutils.pas',
  vclutils in 'utils\vclutils.pas',
  UnitFormPartiesNodes in 'UnitFormPartiesNodes.pas',
  UnitFormSettings in 'UnitFormSettings.pas' {FormSettings},
  UnitFormConsole in 'UnitFormConsole.pas' {FormConsole},
  UnitFormCurrentWork in 'UnitFormCurrentWork.pas' {FormCurrentWork},
  UnitFormDelay in 'UnitFormDelay.pas' {FormDelay},
  UnitFormParties in 'UnitFormParties.pas' {FormParties},
  virtual_tree_node in 'utils\virtual_tree_node.pas',
  DebugEngine.AsmRegUtils in 'DebugEngine\DebugEngine.AsmRegUtils.pas',
  DebugEngine.Core in 'DebugEngine\DebugEngine.Core.pas',
  DebugEngine.DebugInfo in 'DebugEngine\DebugEngine.DebugInfo.pas',
  DebugEngine.DebugUtils in 'DebugEngine\DebugEngine.DebugUtils.pas',
  DebugEngine.Disasm in 'DebugEngine\DebugEngine.Disasm.pas',
  DebugEngine.HookException in 'DebugEngine\DebugEngine.HookException.pas',
  DebugEngine.MemoryHack in 'DebugEngine\DebugEngine.MemoryHack.pas',
  DebugEngine.PeUtils in 'DebugEngine\DebugEngine.PeUtils.pas',
  DebugEngine.Trace in 'DebugEngine\DebugEngine.Trace.pas',
  UnivDisasm.Cnsts.Instructions in 'Common\UnivDisasm\UnivDisasm.Cnsts.Instructions.pas',
  UnivDisasm.Cnsts.Mnemonics in 'Common\UnivDisasm\UnivDisasm.Cnsts.Mnemonics.pas',
  UnivDisasm.Cnsts in 'Common\UnivDisasm\UnivDisasm.Cnsts.pas',
  UnivDisasm.Cnsts.Regs in 'Common\UnivDisasm\UnivDisasm.Cnsts.Regs.pas',
  UnivDisasm.Disasm in 'Common\UnivDisasm\UnivDisasm.Disasm.pas',
  UnivDisasm.Internal.Common in 'Common\UnivDisasm\UnivDisasm.Internal.Common.pas',
  UnivDisasm.Internal.Escape in 'Common\UnivDisasm\UnivDisasm.Internal.Escape.pas',
  UnivDisasm.Internal.Prefixes in 'Common\UnivDisasm\UnivDisasm.Internal.Prefixes.pas',
  UnivDisasm.Syntax.NilSyntax in 'Common\UnivDisasm\UnivDisasm.Syntax.NilSyntax.pas',
  UnivDisasm.Syntax.UnivSyntax in 'Common\UnivDisasm\UnivDisasm.Syntax.UnivSyntax.pas',
  UnivDisasm.Syntax.Utils in 'Common\UnivDisasm\UnivDisasm.Syntax.Utils.pas',
  UnivDisasm.SyntaxManager in 'Common\UnivDisasm\UnivDisasm.SyntaxManager.pas',
  UnivDisasm.Utils in 'Common\UnivDisasm\UnivDisasm.Utils.pas',
  findproc in 'utils\findproc.pas',
  PropertiesFormUnit in 'PropertyGrid\PropertiesFormUnit.pas' {PropertiesForm},
  PropertyValueEditors in 'PropertyGrid\PropertyValueEditors.pas',
  PropertyUtils in 'PropertyGrid\PropertyUtils.pas';

{$R *.res}

begin
    System.ReportMemoryLeaksOnShutdown := false;
    Application.Initialize;
    Application.MainFormOnTaskbar := True;
    Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TFormNewPartyDialog, FormNewPartyDialog);
  Application.CreateForm(TFormPopup, FormPopup);
  Application.CreateForm(TFormManualControl, FormManualControl);
  Application.CreateForm(TFormSettings, FormSettings);
  Application.CreateForm(TFormConsole, FormConsole);
  Application.CreateForm(TFormCurrentWork, FormCurrentWork);
  Application.CreateForm(TFormDelay, FormDelay);
  Application.CreateForm(TFormParties, FormParties);
  Application.CreateForm(TPropertiesForm, PropertiesForm);
  Application.Run;
end.
