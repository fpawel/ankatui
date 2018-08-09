program ankat;

uses
    Vcl.Forms, Vcl.Controls,
    Unit1 in 'Unit1.pas' {Form1} ,
    UnitFormNewPartyDialog
      in 'UnitFormNewPartyDialog.pas' {FormNewPartyDialog} ,
    pipe in 'utils\pipe.pas',
    richeditutils in 'utils\richeditutils.pas',
    stringgridutils in 'utils\stringgridutils.pas',
    listports in 'utils\listports.pas',
    UnitFrameSettings in 'UnitFrameSettings.pas' {FrameSettings: TFrame} ,
    stringutils in 'utils\stringutils.pas',
    UnitData in 'UnitData.pas' {DataModule1: TDataModule} ,
    UnitFrameDelay in 'UnitFrameDelay.pas' {FrameDelay: TFrame} ,
    UnitFormLogNodes in 'UnitFormLogNodes.pas',
    CurrentWorkTreeData in 'CurrentWorkTreeData.pas',
    settings in 'settings.pas',
    UnitFormPopup in 'UnitFormPopup.pas' {FormPopup} ,
    UnitFormLog in 'UnitFormLog.pas' {FormLog} ,
    UnitFrameCoef in 'UnitFrameCoef.pas' {FrameCoef: TFrame} ,
    UnitFrameVar in 'UnitFrameVar.pas' {FrameVar: TFrame} ,
    UnitFormManualControl in 'UnitFormManualControl.pas' {FormManualControl} ,
    UnitFormChart in 'UnitFormChart.pas' {FormChart} ,
    UnitFormChartNodes in 'UnitFormChartNodes.pas',
    msglevel in 'msglevel.pas',
    parties in 'parties.pas',
    variantutils in 'utils\variantutils.pas',
    vclutils in 'utils\vclutils.pas',
    UnitFormPartiesNodes in 'UnitFormPartiesNodes.pas',
    UnitFormSettings in 'UnitFormSettings.pas' {FormSettings} ,
    UnitFormConsole in 'UnitFormConsole.pas' {FormConsole} ,
    UnitFormCurrentWork in 'UnitFormCurrentWork.pas' {FormCurrentWork} ,
    UnitFormDelay in 'UnitFormDelay.pas' {FormDelay};

{$R *.res}

begin
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
    // Application.CreateForm(TFormCurrentWork, FormCurrentWork);
    // Application.CreateForm(TFormParties, FormParties);
    Application.Run;

end.
