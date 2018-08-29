unit config;

interface

uses FireDAC.Stan.Param;

type
    PConfigProperty = ^TConfigProperty;

    TConfigProperty = class
    public
        FSectionName: string;
        FHint: string;
        FValue: string;
        FError: string;
        FPropertyName: string;
        FDefaultValue: String;
        FMin: double;
        FMax: double;
        FMinSet: boolean;
        FMaxSet: boolean;
        FSortOrder: integer;
        FType: string;
        FList: TArray<string>;

        procedure SetStr(str: string);
        procedure SetParam(p: TFDParam);
    end;

    TConfigSection = class
    public
        FSectionName: string;
        FHint: string;
        FSortOrder: integer;
        FProperties: TArray<TConfigProperty>;
        function HasError: boolean;
    end;

    TConfig = TArray<TConfigSection>;

const
    VtcInt = 'integer';
    VtcFloat = 'real';
    VtcString = 'text';
    VtcComportName = 'comport_name';
    VtcBaud = 'baud';
    VtcBool = 'bool';

implementation

uses stringutils, sysutils;

procedure TConfigProperty.SetParam(p: TFDParam);
begin
    if (FType = VtcInt) or (FType = VtcBaud) or (FType = VtcBool) then
        p.AsInteger := strtoint(FValue)
    else if FType = VtcFloat then
        p.AsFloat := str_to_float(FValue)
    else if FType = VtcString then
        p.AsString := FValue
    else
        raise Exception.Create('unknown value type: "' + FType + '"');

end;

function TConfigSection.HasError: boolean;
var
    i: integer;
begin
    for i := 0 to length(self.FProperties) - 1 do
        if FProperties[i].FError <> '' then
            exit(true);
    exit(false);
end;

procedure TConfigProperty.SetStr(str: string);
var
    v: double;
    i, vInt: integer;
    ok: boolean;
begin
    FError := '';
    str := str_validate_decimal_separator(str).Trim;
    FValue := str;
    if str = '' then
    begin
        FError := 'нет значения';
        exit;
    end;

    if length(FList) > 0 then
    begin
        ok := false;
        for i := 0 to length(FList) - 1 do
        begin
            if FList[i] = str then
                ok := true;
        end;
        if not ok then
        begin
            FError := 'значение должно быть из списка: ' + FList[0];
            for i := 1 to length(FList) - 1 do
                FError := FError + '; ' + FList[i];
            exit;
        end;
    end;

    ok := true;
    if (FType = VtcInt) or (FType = VtcBaud) or (FType = VtcBool) then
    begin
        ok := TryStrToInt(str, vInt);
        v := vInt;
        if not ok then
            FError := 'не правильный синтаксис целого числа';
    end
    else if FType = VtcFloat then
    begin
        ok := TryStrToFloat(str, v);
        if not ok then
            FError := 'не правильный синтаксис числа c плавающей точкой';
    end;

    if ok then
    begin
        if FMinSet and (v < FMin) then
            FError := 'меньше ' + floattostr(FMin)
        else if FMaxSet and (v > FMax) then
            FError := 'больше ' + floattostr(FMax);
    end;

end;

end.
