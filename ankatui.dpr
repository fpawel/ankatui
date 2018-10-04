program ankatui;

uses
  Vcl.Forms,
  Vcl.Controls,
  Unit1 in 'Unit1.pas' {Form1},
  UnitFormNewPartyDialog in 'new_party\UnitFormNewPartyDialog.pas' {FormNewPartyDialog},
  pipe in 'utils\pipe.pas',
  richeditutils in 'utils\richeditutils.pas',
  stringgridutils in 'utils\stringgridutils.pas',
  listports in 'utils\listports.pas',
  stringutils in 'utils\stringutils.pas',
  UnitData in 'UnitData.pas' {DataModule1: TDataModule},
  UnitFormLogNodes in 'UnitFormLogNodes.pas',
  CurrentWorkTreeData in 'CurrentWorkTreeData.pas',
  UnitFormPopup in 'UnitFormPopup.pas' {FormPopup},
  UnitFormLog in 'UnitFormLog.pas' {FormLog},
  UnitFormManualControl in 'UnitFormManualControl.pas' {FormManualControl},
  UnitFormCharts in 'UnitFormCharts.pas' {FormCharts},
  UnitFormChartsNodes in 'UnitFormChartsNodes.pas',
  msglevel in 'msglevel.pas',
  variantutils in 'utils\variantutils.pas',
  vclutils in 'utils\vclutils.pas',
  UnitFormPartiesNodes in 'UnitFormPartiesNodes.pas',
  UnitFormSettings in 'UnitFormSettings.pas' {FormSettings},
  UnitFormConsole in 'UnitFormConsole.pas' {FormConsole},
  UnitFormCurrentWork in 'UnitFormCurrentWork.pas' {FormCurrentWork},
  UnitFormDelay in 'UnitFormDelay.pas' {FormDelay},
  UnitFormParties in 'UnitFormParties.pas' {FormParties},
  virtual_tree_node in 'utils\virtual_tree_node.pas',
  findproc in 'utils\findproc.pas',
  PropertiesFormUnit in 'PropertyGrid\PropertiesFormUnit.pas' {PropertiesForm},
  PropertyValueEditors in 'PropertyGrid\PropertyValueEditors.pas',
  PropertyUtils in 'PropertyGrid\PropertyUtils.pas',
  UnitHostAppData in 'UnitHostAppData.pas' {HostAppData: TDataModule},
  config in 'config\config.pas',
  DataRichEditOutput in 'DataRichEditOutput.pas',
  UnitFormChartSeries in 'UnitFormChartSeries.pas' {FormChartSeries},
  models in 'models.pas',
  UnitFormReadVars in 'UnitFormReadVars.pas' {FormReadVars},
  UnitFromCoefs in 'UnitFromCoefs.pas' {FormCoefs},
  HostAppModels in 'HostAppModels.pas';

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
  Application.CreateForm(THostAppData, HostAppData);
  Application.CreateForm(TFormChartSeries, FormChartSeries);
  Application.CreateForm(TFormReadVars, FormReadVars);
  Application.CreateForm(TFormCharts, FormCharts);
  Application.CreateForm(TFormLog, FormLog);
  Application.CreateForm(TFormCoefs, FormCoefs);
  Application.Run;

end.
