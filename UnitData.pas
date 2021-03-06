unit UnitData;

interface

uses
    System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
    FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
    FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
    FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.VCLUI.Wait, Data.DB,
    FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
    FireDAC.DApt, Vcl.ComCtrls, FireDAC.Comp.DataSet,
    System.Generics.collections, config, models;

type

    TQueryHandler = reference to procedure(q: TFDQuery);

    TDataModule1 = class(TDataModule)
        FDConnectionProductsDB: TFDConnection;
        FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
        FDQueryPartyWorks: TFDQuery;
        FDQueryWorkMessages: TFDQuery;
        FDConnectionConfig: TFDConnection;
        FDQueryUpdateCoefValue: TFDQuery;
        FDQueryDeleteCoefValue: TFDQuery;
        FDQueryPartyWorksDays: TFDQuery;
        FDQueryCurrentWorkMessages: TFDQuery;
        FDQueryPartyProductsWithCoefs: TFDQuery;
        FDQueryPartyCoefsWithProducts: TFDQuery;
        FDQueryDayLog: TFDQuery;
        FDQueryWorkLogYears: TFDQuery;
        FDQueryWorkLogYearMonths: TFDQuery;
        FDQueryWorkLogYearMonthDays: TFDQuery;
        FDQueryWorkLogsYearMonthDay: TFDQuery;
        FDQuery1: TFDQuery;
        procedure DataModuleCreate(Sender: TObject);
        procedure FDConnectionProductsDBError(ASender, AInitiator: TObject;
          var AException: Exception);
    private
        { Private declarations }
        FVars: TArray<TDeviceVar>;
        procedure read_config_section(sect: TConfigSection);

    public
        { Public declarations }

        function GetDeviceVarName(v: integer): string;
        function GetDeviceVarByName(s: string): integer;
        function DeviceCoefs: TArray<TDeviceVar>;
        function PartiesYears: TArray<integer>;
        function SeriesYears: TArray<integer>;
        function PartyValues(partyID: int64): TKeysValues;
        function CurrentPartyProducts: TArray<TProduct>;
        function CurrentPartyID: int64;
        function PartyExists: boolean;
        procedure UpdateProductComport(Ordinal: integer; comport: string);
        procedure UpdateProductchecked(Ordinal: integer; checked: boolean);
        procedure UpdateCurrentWorkCheckState(Ordinal: integer;
          checkState: string);
        function GetCurrentWorkCheckState(Ordinal: integer): integer;

        procedure UpdateDeviceVarChecked(n_var: integer; checked: boolean);
        procedure InvertDeviceVarChecked(n_var: integer);

        function InvertProductsChecked: boolean;
        function InvertVarsChecked: boolean;

        function CurrentPartyCoefs: TArray<RProductCoefValue>;
        procedure UpdateCoefChecked(Coef: integer; checked: boolean);

        function InvertCoefsChecked: boolean;
        function GetCoefValue(product_ordinal, Coef: integer): string;
        procedure SetCoefValue(product_ordinal, Coef: integer; Value: string);

        function PartyProductsWithCoefs(partyID: int64): TArray<integer>;
        function PartyCoefsWithProducts(partyID: int64): TArray<integer>;
        function ProductCoeffValue(partyID: int64;
          Serial, Coef: integer): string;
        function PartyProducts(partyID: int64): TArray<integer>;

        function GetSeriesVarProducts(seriesID: int64; AVar: integer)
          : TArray<integer>;

        procedure UpdateConfig(p: TConfigProperty);

        function GetConfig: TConfig;

        function GetConfigPropertyValue(section_name, property_name
          : string): variant;
        function GetConfigPropertyDefaultValue(section_name,
          property_name: string): variant;

        procedure NewParty(AParty: TConfigSection; serials: TArray<integer>);

        function CurrentPartyDateTime: TDAteTime;

        procedure SetWorksChecked(xs: array of integer);

        function GetConfigPropertyValueList(property_name: string)
          : TArray<string>;

        property DeviceVars: TArray<TDeviceVar> read FVars;

    end;

var
    DataModule1: TDataModule1;

implementation

uses dateutils, Vcl.dialogs, System.Variants, stringutils, variantutils;

{ %CLASSGROUP 'Vcl.Controls.TControl' }

{$R *.dfm}

procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin
    if GetEnvironmentVariable('MYAPPDATA') = '' then
    begin
        FDConnectionProductsDB.Params.Database :=
          '$(APPDATA)\������������\ankat\products.db';
        FDConnectionConfig.Params.Database :=
          '$(APPDATA)\������������\ankat\config.db';

    end;

    FDConnectionProductsDB.Connected := true;
    FDConnectionConfig.Connected := true;
    SetLength(FVars, 0);

    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text := 'SELECT * FROM read_var ORDER BY var;';
        Open;
        First;
        while not Eof do
        begin
            SetLength(FVars, length(FVars) + 1);
            FVars[length(FVars) - 1] := TDeviceVar.Create;
            with FVars[length(FVars) - 1] do
            begin
                FVar := FieldValues['var'];
                FName := FieldValues['name'];
                FChecked := FieldValues['checked'];
                FDescription := FieldValues['description'];
            end;
            Next;
        end;
        Close;
        Free;
    end;
end;

function TDataModule1.InvertVarsChecked: boolean;
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text :=
          'SELECT exists(SELECT * FROM read_var WHERE checked = 1) as checked;';
        Open;
        First;
        result := FieldValues['checked'];
        result := not result;
        Close;

        SQL.Text := 'UPDATE read_var SET checked = :checked;';
        ParamByName('checked').Value := result;
        ExecSQL;
        Free;
    end;
end;

function TDataModule1.InvertCoefsChecked: boolean;
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text :=
          'SELECT exists(SELECT * FROM coefficient WHERE checked = 1) as checked;';
        Open;
        First;
        result := FieldValues['checked'];
        result := not result;
        Close;

        SQL.Text := 'UPDATE coefficient SET checked = :checked;';
        ParamByName('checked').Value := result;
        ExecSQL;
        Free;
    end;
end;

function TDataModule1.InvertProductsChecked: boolean;
var
    count, I: integer;

begin
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text :=
          'SELECT exists(SELECT * FROM current_party_products_config WHERE checked = 1) as checked;';
        Open;
        First;
        result := FieldValues['checked'];
        result := not result;
        Close;

        SQL.Text := 'SELECT count(*) as count FROM current_party_products;';
        Open;
        First;
        count := FieldValues['count'];

    end;
    for I := 0 to count - 1 do
        UpdateProductchecked(I, result);

end;

function TDataModule1.GetCurrentWorkCheckState(Ordinal: integer): integer;
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionConfig;
        SQL.Text :=
          'SELECT checked FROM work_checked WHERE work_order = :ordinal;';
        ParamByName('ordinal').Value := Ordinal;
        Open;
        First;
        if not Eof then
            result := FieldValues['checked']
        else
            result := 0;
        Free;
    end;

end;

procedure TDataModule1.UpdateCurrentWorkCheckState(Ordinal: integer;
  checkState: string);
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionConfig;
        SQL.Text :=
          'INSERT OR REPLACE INTO work_checked VALUES (:ordinal, :checked);';
        ParamByName('ordinal').Value := Ordinal;
        ParamByName('checked').Value := checkState;
        ExecSQL;
        Free;
    end;
end;

procedure TDataModule1.UpdateProductComport(Ordinal: integer; comport: string);
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text :=
          'INSERT OR IGNORE INTO product_config(ordinal) VALUES (:ordinal); ' +
          'UPDATE product_config SET comport = :comport WHERE ordinal = :ordinal;';
        ParamByName('ordinal').Value := Ordinal;
        ParamByName('comport').Value := comport;
        ExecSQL;
        Free;
    end;

end;

procedure TDataModule1.UpdateCoefChecked(Coef: integer; checked: boolean);
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text :=
          'UPDATE coefficient SET checked = :checked WHERE coefficient_id = :coefficient_id;';
        ParamByName('coefficient_id').Value := Coef;
        ParamByName('checked').Value := checked;
        ExecSQL;
        Free;
    end;

end;

procedure TDataModule1.InvertDeviceVarChecked(n_var: integer);
begin
    UpdateDeviceVarChecked(n_var, not DeviceVars[n_var].FChecked);
end;

procedure TDataModule1.UpdateDeviceVarChecked(n_var: integer;
  checked: boolean);
begin
    DeviceVars[n_var].FChecked := checked;
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text :=
          'UPDATE read_var SET checked = :checked WHERE var = :devicevar;';
        ParamByName('devicevar').Value := DeviceVars[n_var].FVar;
        ParamByName('checked').Value := checked;
        ExecSQL;
        Free;
    end;

end;

procedure TDataModule1.UpdateProductchecked(Ordinal: integer; checked: boolean);
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text :=
          'INSERT OR IGNORE INTO product_config(ordinal) VALUES (:ordinal); ' +
          'UPDATE product_config SET checked = :checked WHERE ordinal = :ordinal;';
        ParamByName('ordinal').Value := Ordinal;
        ParamByName('checked').Value := checked;
        ExecSQL;
        Free;
    end;

end;

function TDataModule1.PartyExists: boolean;
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text := 'select exists( select party_id from party ) as r';
        Open;
        result := FieldValues['r'];
        Free;
    end;

end;

function TDataModule1.CurrentPartyDateTime: TDAteTime;
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text := 'select * from current_party';
        Open;
        result := FieldValues['created_at'];
        Free;
    end;
end;

function TDataModule1.CurrentPartyID: int64;
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text := 'select party_id from current_party';
        Open;
        result := FieldValues['party_id'];
        Free;
    end;
end;

function TDataModule1.CurrentPartyProducts: TArray<TProduct>;
var
    xs: TList<TProduct>;

begin
    xs := TList<TProduct>.Create;

    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text := 'select * from current_party_products_config';
        Open;
        First;
        while not Eof do
        begin
            xs.Add(TProduct.Create(FieldValues['product_serial'],
              FieldValues['checked'], FieldValues['comport']));
            Next;
        end;
        Free;
    end;
    result := xs.ToArray;
    xs.Free;

end;

function TDataModule1.DeviceCoefs: TArray<TDeviceVar>;
var
    xs: TList<TDeviceVar>;
    v: TDeviceVar;
begin
    xs := TList<TDeviceVar>.Create;
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text := 'SELECT * FROM coefficient ORDER BY coefficient_id;';
        Open;
        First;
        while not Eof do
        begin
            v := TDeviceVar.Create;
            v.FVar := FieldValues['coefficient_id'];
            v.FName := FieldValues['name'];
            v.FChecked := FieldValues['checked'];
            v.FDescription := FieldValues['description'];
            xs.Add(v);
            Next;
        end;
        Close;
        Free;
    end;
    result := xs.ToArray;
    xs.Free;

end;

function TDataModule1.GetDeviceVarByName(s: string): integer;
var
    v: TDeviceVar;
begin
    for v in FVars do
    begin
        if v.FName = s then
        begin
            Exit(v.FVar);
        end;
    end;
    Exit(-1);
end;

function TDataModule1.GetDeviceVarName(v: integer): string;
var
    x: TDeviceVar;
begin
    for x in FVars do
        if x.FVar = v then
            Exit(x.FName);
    Exit('');
end;

procedure TDataModule1.FDConnectionProductsDBError(ASender, AInitiator: TObject;
  var AException: Exception);
begin
    ApplicationShowException
      (Exception.Create(FDConnectionProductsDB.Params.Database + ': ' +
      AException.Message))
end;

function TDataModule1.PartyValues(partyID: int64): TKeysValues;
begin

    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionConfig;
        SQL.Text :=
          'SELECT property_name FROM config WHERE section_name = :party;';
        ParamByName('party').Value := 'party';
        Open;
        First;
        while not Eof do
        begin
            SetLength(result, length(result) + 1);
            with result[length(result) - 1] do
            begin
                Key := FieldValues['property_name'];

                with TFDQuery.Create(nil) do
                begin
                    Connection := FDConnectionProductsDB;
                    SQL.Text := 'SELECT ' + Key +
                      ' FROM party WHERE party_id = :party_id;';
                    ParamByName('party_id').Value := partyID;
                    Open;
                    First;
                    if Eof then
                        raise Exception.Create('Not found: ' + Key);
                    Value := FieldValues[Key];
                    Close;
                    Free;
                end;

            end;
            Next;
        end;

        Close;
        Free;
    end;
end;

function TDataModule1.SeriesYears: TArray<integer>;
var
    xs: TList<integer>;
begin
    xs := TList<integer>.Create;
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text := 'SELECT DISTINCT year FROM series_info;';
        Open;
        First;
        while not Eof do
        begin
            xs.Add(FieldValues['year']);
            Next;
        end;
        Close;
        Free;
    end;
    if xs.count = 0 then
        xs.Add(YearOf(now));
    result := xs.ToArray;
    xs.Free;
end;

function TDataModule1.PartiesYears: TArray<integer>;
var
    xs: TList<integer>;
begin
    xs := TList<integer>.Create;
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text := 'SELECT * FROM party_year;';
        Open;
        First;
        while not Eof do
        begin

            xs.Add(FieldValues['year']);
            Next;
        end;
        Close;
        Free;
    end;
    result := xs.ToArray;
    xs.Free;
end;

function TDataModule1.CurrentPartyCoefs: TArray<RProductCoefValue>;
var
    xs: TList<RProductCoefValue>;
    x: RProductCoefValue;
begin
    xs := TList<RProductCoefValue>.Create;

    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text := 'SELECT * FROM current_party_coefficient_value;';
        Open;
        First;
        while not Eof do
        begin
            x.Serial := FieldValues['product_serial'];
            x.Ordinal := FieldValues['ordinal'];
            x.Coef := FieldValues['coefficient_id'];
            x.Value := FieldValues['value'];
            xs.Add(x);
            Next;
        end;
        Close;
        Free;
    end;

    result := xs.ToArray;
    xs.Free;
end;

function TDataModule1.GetCoefValue(product_ordinal, Coef: integer): string;
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text := 'SELECT value FROM current_party_coefficient_value ' +
          'WHERE ordinal=:ordinal AND coefficient_id = :coef;';
        ParamByName('ordinal').Value := product_ordinal;
        ParamByName('coef').Value := Coef;
        Open;
        First;
        if not Eof then
            result := FloatToStr(FieldValues['value']);
        Free;
    end;
end;

procedure TDataModule1.SetCoefValue(product_ordinal, Coef: integer;
  Value: string);
var
    v: extended;
    q: TFDQuery;
begin
    Value := str_validate_decimal_separator(Value);
    if TryStrToFloat(Value, v) then
    begin
        q := FDQueryUpdateCoefValue;
        q.ParamByName('value').Value := v;
    end
    else
        q := FDQueryDeleteCoefValue;
    with q do
    begin
        ParamByName('ordinal').Value := product_ordinal;
        ParamByName('coef').Value := Coef;
        ExecSQL;
    end

end;

function TDataModule1.PartyProducts(partyID: int64): TArray<integer>;
var
    xs: TList<integer>;
begin
    xs := TList<integer>.Create;
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text :=
          'SELECT product_serial FROM product WHERE party_id = :party_id';
        ParamByName('party_id').Value := partyID;
        Open;
        First;
        while not Eof do
        begin
            xs.Add(FieldValues['product_serial']);
            Next;
        end;
        Close;
        Free;
    end;
    result := xs.ToArray;
    xs.Free;
end;

function TDataModule1.PartyProductsWithCoefs(partyID: int64): TArray<integer>;
var
    xs: TList<integer>;
begin
    xs := TList<integer>.Create;
    with FDQueryPartyProductsWithCoefs do
    begin
        ParamByName('party_id').Value := partyID;
        Open;
        First;
        while not Eof do
        begin
            xs.Add(FieldValues['product_serial']);
            Next;
        end;
        Close
    end;
    result := xs.ToArray;
    xs.Free;
end;

function TDataModule1.PartyCoefsWithProducts(partyID: int64): TArray<integer>;
var
    xs: TList<integer>;
begin
    xs := TList<integer>.Create;
    with FDQueryPartyCoefsWithProducts do
    begin
        ParamByName('party_id').Value := partyID;
        Open;
        First;
        while not Eof do
        begin
            xs.Add(FieldValues['coefficient_id']);
            Next;
        end;
        Close
    end;
    result := xs.ToArray;
    xs.Free;
end;

function TDataModule1.ProductCoeffValue(partyID: int64;
  Serial, Coef: integer): string;

begin
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text :=
          'SELECT value FROM product_coefficient_value WHERE party_id = :party_id '
          + 'AND product_serial = :product_serial ' +
          'AND coefficient_id = :coefficient_id;';
        ParamByName('party_id').Value := partyID;
        ParamByName('product_serial').Value := Serial;
        ParamByName('coefficient_id').Value := Coef;
        Open;
        First;
        if not Eof then
            result := VarToStr(FieldValues['value'])
        else
            result := '';
        Free;
    end;
end;

function TDataModule1.GetSeriesVarProducts(seriesID: int64; AVar: integer)
  : TArray<integer>;
var
    xs: TList<integer>;
begin
    xs := TList<integer>.Create;
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text := 'SELECT DISTINCT product_serial FROM chart_value_info ' +
          'WHERE series_id = :series_id AND var = :var;';
        ParamByName('series_id').Value := seriesID;
        ParamByName('var').Value := AVar;
        Open;
        First;
        while not Eof do
        begin
            xs.Add(FieldValues['product_serial']);
            Next;
        end;
        Free;
    end;
    result := xs.ToArray;
    xs.Free;
end;

procedure TDataModule1.UpdateConfig(p: TConfigProperty);
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := DataModule1.FDConnectionConfig;

        if p.FSectionName = 'party' then
        begin
            Connection := DataModule1.FDConnectionProductsDB;
            SQL.Text := 'UPDATE party SET ' + p.FPropertyName + ' = :value ' +
              'WHERE party_id IN (SELECT party_id FROM current_party);';
        end
        else
        begin
            Connection := DataModule1.FDConnectionConfig;
            SQL.Text := 'UPDATE config SET value = :value where ' +
              'section_name = :section_name AND ' +
              'property_name = :property_name ;';
            ParamByName('property_name').Value := p.FPropertyName;
            ParamByName('section_name').Value := p.FSectionName;
        end;
        p.SetParam(ParamByName('value'));

        ExecSQL;
        Close;
        Free;
    end;
end;

function TDataModule1.GetConfigPropertyValue(section_name,
  property_name: string): variant;
begin
    with TFDQuery.Create(nil) do
    begin
        if section_name = 'party' then
        begin
            Connection := FDConnectionProductsDB;
            SQL.Text := 'SELECT ' + property_name + ' FROM current_party;';
            Open;
            First;
            if not Eof then
                result := FieldValues[property_name];
        end
        else
        begin
            Connection := FDConnectionConfig;
            SQL.Text :=
              'SELECT value FROM config WHERE section_name = :section_name AND property_name = :property_name;';
            ParamByName('section_name').Value := section_name;
            ParamByName('property_name').Value := property_name;
            Open;
            First;
            if not Eof then
                result := FieldValues['value'];
        end;

        Close;
        Free;
    end;
end;

function TDataModule1.GetConfigPropertyDefaultValue(section_name,
  property_name: string): variant;
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionConfig;
        SQL.Text :=
          'SELECT default_value FROM config WHERE section_name = :section_name AND property_name = :property_name;';
        ParamByName('section_name').Value := section_name;
        ParamByName('property_name').Value := property_name;
        Open;
        First;
        if not Eof then
            result := FieldValues['default_value'];
        Close;
        Free;
    end;
end;

function TDataModule1.GetConfigPropertyValueList(property_name: string)
  : TArray<string>;
begin
    SetLength(result, 0);
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionConfig;
        SQL.Text :=
          'select value from value_list where property_name = :property_name;';
        ParamByName('property_name').Value := property_name;
        Open;
        First;
        while not Eof do
        begin
            SetLength(result, length(result) + 1);
            result[length(result) - 1] := FieldValues['value'];
            Next;
        end;
        Close;
        Free;
    end;

end;

procedure TDataModule1.read_config_section(sect: TConfigSection);
var
    config_property: TConfigProperty;
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionConfig;

        SQL.Text :=
          'SELECT * FROM config WHERE section_name = :section_name ORDER BY sort_order;';
        ParamByName('section_name').Value := sect.FSectionName;
        Open;
        First;
        while not Eof do
        begin
            SetLength(sect.FProperties, length(sect.FProperties) + 1);
            sect.FProperties[length(sect.FProperties) - 1] :=
              TConfigProperty.Create;
            with sect.FProperties[length(sect.FProperties) - 1] do
            begin
                FSectionName := sect.FSectionName;
                FType := FieldValues['type'];
                FPropertyName := FieldValues['property_name'];
                FHint := FieldValues['hint'];
                FSortOrder := FieldValues['sort_order'];
                FMinSet := FieldValues['min'] <> System.Variants.Null;
                FMaxSet := FieldValues['max'] <> System.Variants.Null;
                if FMinSet then
                    FMin := FieldValues['min'];
                if FMaxSet then
                    FMax := FieldValues['max'];

                FValue := GetConfigPropertyValue(FSectionName, FPropertyName);
                FDefaultValue := GetConfigPropertyDefaultValue(FSectionName,
                  FPropertyName);

                FList := GetConfigPropertyValueList(FPropertyName);

                SetStr(FValue);
            end;
            Next;
        end;
        Close;
        Free;

    end;
end;

function TDataModule1.GetConfig: TConfig;
begin

    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionConfig;
        SQL.Text := 'SELECT * FROM section;';
        Open;
        First;
        while not Eof do
        begin
            SetLength(result, length(result) + 1);
            result[length(result) - 1] := TConfigSection.Create;
            with result[length(result) - 1] do
            begin
                FSectionName := FieldByName('section_name').Value;
                FSortOrder := FieldByName('sort_order').Value;
                FHint := FieldByName('hint').Value;
            end;

            read_config_section(result[length(result) - 1]);

            Next;
        end;
        Close;
        Free;
    end;
end;

procedure TDataModule1.NewParty(AParty: TConfigSection;
  serials: TArray<integer>);
var
    p: TConfigProperty;
    Serial: integer;
    s: string;
begin

    with TFDQuery.Create(nil) do
    begin
        Connection := DataModule1.FDConnectionProductsDB;
        SQL.Text := 'INSERT INTO party DEFAULT VALUES;';
        Execute;

        for p in AParty.FProperties do
        begin
            s := 'UPDATE party SET ' + p.FPropertyName + ' = :value ' +
              'WHERE party_id IN (SELECT party_id FROM current_party);';
            SQL.Text := s;
            p.SetParam(ParamByName('value'));
            Execute;
        end;

        for Serial in serials do
        begin
            SQL.Text := 'INSERT INTO product (party_id, product_serial) VALUES '
              + '((SELECT party_id FROM current_party), :serial)';
            ParamByName('serial').Value := Serial;
            Execute;
        end;
        Close;
        Free;
    end;
end;

procedure TDataModule1.SetWorksChecked(xs: array of integer);
var
    I: integer;
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := DataModule1.FDConnectionConfig;
        SQL.Text := 'INSERT OR REPLACE INTO work_checked VALUES ';
        for I := 0 to length(xs) - 1 do
        begin
            if I > 0 then
                SQL.Text := SQL.Text + ', ';
            SQL.Text := SQL.Text + format('(%d, %d)', [I, xs[I]]);
        end;
        Execute;
        Close;
        Free;
    end;
end;

end.
