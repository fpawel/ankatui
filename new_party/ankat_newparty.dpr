program ankat_newparty;

uses
  Vcl.Forms,
  UnitData in '..\UnitData.pas' {DataModule1: TDataModule},
  UnitFormNewPartyDialog in 'UnitFormNewPartyDialog.pas' {FormNewPartyDialog},
  PropertiesFormUnit in '..\PropertyGrid\PropertiesFormUnit.pas' {PropertiesForm},
  PropertyUtils in '..\PropertyGrid\PropertyUtils.pas',
  PropertyValueEditors in '..\PropertyGrid\PropertyValueEditors.pas',
  config in '..\config\config.pas',
  stringutils in '..\utils\stringutils.pas',
  listports in '..\utils\listports.pas',
  stringgridutils in '..\utils\stringgridutils.pas',
  variantutils in '..\utils\variantutils.pas',
  models in '..\models.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TDataModule1, DataModule1);
  Application.CreateForm(TFormNewPartyDialog, FormNewPartyDialog);
  Application.Run;
end.
