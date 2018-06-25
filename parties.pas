unit parties;

interface

uses UnitFrameSettings, FireDAC.Comp.Client;

function PartyValuesSection(FDQueryProducts, FDQueryConfig: TFDQuery)
  : TConfigSection;

implementation

uses System.Generics.Collections, System.variants;

function PartyValuesSection(FDQueryProducts, FDQueryConfig: TFDQuery)
  : TConfigSection;
var
    p: UnitFrameSettings.TConfigValue;
    I: Integer;
    vars: TList<TConfigValue>;
    value_list: TList<string>;
begin
    result := TConfigSection.Create;
    vars := TList<TConfigValue>.Create;
    result.FName := 'Параметры партии';
    with FDQueryProducts do
    begin
        SQL.Text := 'select * FROM  party_var;';
        Open;
        First;
        while not Eof do
        begin
            vars.add(UnitFrameSettings.TConfigValue.Create);
            with vars.Last do
            begin
                FVar := FieldValues['var'];
                FName := FieldValues['name'];
                FSection := result.FName;
                FSortOrder := FieldValues['sort_order'];
                FMinSet := FieldValues['min'] <> System.variants.Null;
                FMaxSet := FieldValues['max'] <> System.variants.Null;
                if FMinSet then
                    FMin := FieldValues['min'];
                if FMaxSet then
                    FMax := FieldValues['max'];
                FType := FieldValues['type'];
                if FieldValues['def_val'] <> System.variants.Null then
                    FValue := VarToStr(FieldValues['def_val']);
            end;
            Next;
        end;
        Close;
        for p in vars do
            with FDQueryConfig do
            begin
                SQL.Text :=
                  'select value from value_list where var = :var;';
                ParamByName('var').Value := p.FVar;
                Open;
                First;
                value_list := TList<string>.Create;
                while not Eof do
                begin
                    value_list.add(FieldValues['value']);
                    Next;
                end;
                Close;
                p.FList := value_list.ToArray;
                if value_list.Count > 0 then
                    p.FValue := value_list[0];
                value_list.Free;
            end;
    end;
    result.FItems := vars.ToArray;
    vars.Free;
end;

end.
