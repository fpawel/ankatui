unit UnitFormSettings;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, PropertiesFormUnit;

type
    TFormSettings = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormDeactivate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    private
        { Private declarations }
        FPropertiesForm : TPropertiesForm;

    public
        { Public declarations }
    end;

var
    FormSettings: TFormSettings;

implementation

{$R *.dfm}

uses UnitData, config;


procedure TFormSettings.FormActivate(Sender: TObject);
begin
    FPropertiesForm.SetConfig(DataModule1.GetConfig);
end;

procedure TFormSettings.FormCreate(Sender: TObject);
begin

    FPropertiesForm := TPropertiesForm.Create(self);
    with FPropertiesForm do
    begin
        Parent := self;
        Align := alClient;
        BorderStyle := bsNone;
        Visible := true;
        SetPropertyValueChanged(procedure(p:TConfigProperty)
        begin
            if p.FError = '' then
                DataModule1.UpdateConfig(p);
        end);
    end;
end;

procedure TFormSettings.FormDeactivate(Sender: TObject);
begin
    Hide;
end;

procedure TFormSettings.FormHide(Sender: TObject);
begin
    FPropertiesForm.VST3.CancelEditNode;
end;

end.
