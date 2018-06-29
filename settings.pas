unit settings;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
    Vcl.ExtCtrls, Vcl.StdCtrls, UnitFrameSettings;

type
    TSettingsControl = class
        FConfig: TConfig;
        FFrameSettings: TFrameSettings;

        constructor Create;
        procedure Validate;

    end;

implementation

uses parties, System.Generics.Collections, UnitData, FireDAC.Stan.PAram,
    stringutils;

procedure readSectionItems(sect: TConfigSection);
var
    vars: TList<TConfigValue>;
    value_list: TList<string>;
begin
    vars := TList<TConfigValue>.Create;
    with DataModule1.FDQueryConfig2 do
    begin

        SQL.Text :=
          'SELECT * FROM config WHERE section_name = :section ORDER BY sort_order;';
        ParamByName('section').Value := sect.FName;
        Open;
        First;
        while not Eof do
        begin
            vars.add(TConfigValue.Create);
            with vars.Last do
            begin
                FSection := sect.FName;
                FType := FieldValues['type'];
                FValue := VarToStr(FieldValues['value']);
                FVar := FieldValues['var'];
                FName := FieldValues['name'];
                FSortOrder := FieldValues['sort_order'];
                FMinSet := FieldValues['min'] <> System.Variants.Null;
                FMaxSet := FieldValues['max'] <> System.Variants.Null;
                if FMinSet then
                    FMin := FieldValues['min'];
                if FMaxSet then
                    FMax := FieldValues['max'];

                value_list := TList<string>.Create;
                with DataModule1.FDQueryConfig3 do
                begin
                    SQL.Text :=
                      'select value from value_list where var = :var;';
                    ParamByName('var').Value := FVar;
                    Open;
                    First;
                    while not Eof do
                    begin
                        value_list.add(FieldValues['value']);
                        Next;
                    end;
                    Close;

                end;
                FList := value_list.ToArray;
                value_list.Free;
            end;
            Next;
        end;
        Close;
        sect.FItems := vars.ToArray;
    end;
    vars.Free;
end;

constructor TSettingsControl.Create;
var
    sections: TList<TConfigSection>;

    sect: TConfigSection;
begin
    inherited Create;
    FFrameSettings := TFrameSettings.Create(nil);
    FConfig := TConfig.Create;
    sections := TList<TConfigSection>.Create;
    sections.add(PartyValuesSection(DataModule1.FDQuery1,
      DataModule1.FDQueryConfig));
    with DataModule1.FDQueryConfig do
    begin

        SQL.Text := 'SELECT * FROM section;';
        Open;
        First;
        while not Eof do
        begin
            sect := TConfigSection.Create;
            sect.FName := FieldByName('name').Value;
            sect.FSortOrder := FieldByName('sort_order').Value;
            readSectionItems(sect);
            sections.add(sect);
            Next;
        end;
        Close;
    end;

    FConfig.FItems := sections.ToArray;
    sections.Free;

    FFrameSettings.SetConfig(FConfig);

    FFrameSettings.OnValueChanged := procedure(p: TConfigValue)
        begin
            if p.FSection = 'Параметры партии' then
                with DataModule1.FDQuery1 do
                begin

                    SQL.Text :=
                      'INSERT OR REPLACE INTO party_value (var, party_id, value) '
                      + 'VALUES (:var, (SELECT * FROM current_party_id), :value);';
                    ParamByName('var').Value := p.FVar;
                    p.SetParam(ParamByName('value'));
                    ExecSQL;
                    Close;
                end
            else
                with DataModule1.FDQueryConfig do
                begin
                    SQL.Text :=
                      'UPDATE config SET value = :value where var = :var;';
                    ParamByName('var').Value := p.FVar;
                    p.SetParam(ParamByName('value'));
                    ExecSQL;
                    Close;
                end;
        end;

end;

procedure TSettingsControl.Validate;
var
    pv: TConfigValue;
    i: integer;
begin

    for i := 0 to length(FConfig.FItems) - 1 do
    begin
        if FConfig.FItems[i].FName = 'Параметры партии' then

            for pv in FConfig.FItems[i].FItems do
                with DataModule1.FDQuery1 do
                begin
                    SQL.Text :=
                      'select value from party_value where party_id in current_party_id and var = :var;';
                    ParamByName('var').Value := pv.FVar;
                    Open;
                    First;
                    pv.FValue := VarToStr(FieldByName('value').Value);
                    Close;
                end
        else

            for pv in FConfig.FItems[i].FItems do
                with DataModule1.FDQueryConfig do
                begin
                    SQL.Text := 'SELECT value FROM config WHERE var  = :var;';
                    ParamByName('var').Value := pv.FVar;
                    Open;
                    First;
                    pv.FValue := VarToStr(FieldByName('value').Value);
                    Close;
                end;

    end;
    FFrameSettings.InvalidateControls;
end;

end.
