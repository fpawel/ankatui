unit UnitData;

interface

uses
    System.SysUtils, System.Classes, FireDAC.Stan.Intf, FireDAC.Stan.Option,
    FireDAC.Stan.Error, FireDAC.UI.Intf, FireDAC.Phys.Intf, FireDAC.Stan.Def,
    FireDAC.Stan.Pool, FireDAC.Stan.Async, FireDAC.Phys, FireDAC.Phys.SQLite,
    FireDAC.Phys.SQLiteDef, FireDAC.Stan.ExprFuncs, FireDAC.VCLUI.Wait, Data.DB,
    FireDAC.Comp.Client, FireDAC.Stan.Param, FireDAC.DatS, FireDAC.DApt.Intf,
    FireDAC.DApt, Vcl.ComCtrls, FireDAC.Comp.DataSet,
    System.Generics.collections;

type
    TKeyValue = TPair<string, variant>;

    TKeysValues = TArray<TKeyValue>;

    TProduct = class
        FSerial: integer;
        FChecked: boolean;
        FComport: string;
        FConnection: string;
        FConnectionError: boolean;

        constructor Create(ASerial: integer; AChecked: boolean;
          AComport: string);
    end;

    RProductVar = record
        FProduct: integer;
        FVar: integer;

    end;

    RValueError = record
        FValue: string;
        FError: boolean;

    end;

    TDeviceVar = class
        FVar: integer;
        FName: string;
        FDescription: string;
        FChecked: boolean;
    end;

    RProductCoefValue = record
        Serial: integer;
        Ordinal: integer;
        Coef: integer;
        Value: double;
    end;

    TProductVarValues = TDictionary<RProductVar, RValueError>;

    TReadVar = class
        FProduct: integer;
        FVar: integer;
        FValue: double;
        FError: string;
        function ProductVar: RProductVar;
        function ValueError: RValueError;
    end;

    TModbusCommand = TPair<integer, string>;

    TDataModule1 = class(TDataModule)
        FDConnectionProductsDB: TFDConnection;
        FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
        FDQueryWorksByParentRecordID: TFDQuery;
        FDQuery1: TFDQuery;
        FDQueryConfig2: TFDQuery;
        FDQueryPartyWorks: TFDQuery;
        FDQueryWorkMessages: TFDQuery;
        FDConnectionConfig: TFDConnection;
        FDQueryConfig: TFDQuery;
        FDQueryConfig3: TFDQuery;
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
        procedure DataModuleCreate(Sender: TObject);
    private
        { Private declarations }
    public
        { Public declarations }
        function DeviceVars: TArray<TDeviceVar>;
        function DeviceCoefs: TArray<TDeviceVar>;
        function PartiesYears: TArray<integer>;
        function SeriesYears: TArray<integer>;
        function PartyValues(partyID: int64): TKeysValues;
        function CurrentPartyProducts: TArray<TProduct>;
        function CurrentPartyID: int64;
        procedure UpdateProductComport(Ordinal: integer; comport: string);
        procedure UpdateProductchecked(Ordinal: integer; checked: boolean);
        procedure UpdateCurrentWorkCheckState(Ordinal: integer;
          checkState: string);
        function GetCurrentWorkCheckState(Ordinal: integer): string;
        procedure UpdateDeviceVarChecked(devicevar: integer; checked: boolean);

        function InvertProductsChecked: boolean;
        function InvertVarsChecked: boolean;

        function CurrentPartyCoefs: TArray<RProductCoefValue>;
        procedure UpdateCoefChecked(Coef: integer; checked: boolean);
        function InvertCoefsChecked: boolean;
        function GetCoefValue(product_ordinal, Coef: integer): string;
        procedure SetCoefValue(product_ordinal, Coef: integer; Value: string);
        function ModbusCommands: TArray<TModbusCommand>;

        function PartyProductsWithCoefs(partyID: int64): TArray<integer>;
        function PartyCoefsWithProducts(partyID: int64): TArray<integer>;
        function ProductCoeffValue(partyID: int64;
          Serial, Coef: integer): string;
        function PartyProducts(partyID: int64): TArray<integer>;

        procedure PrintCurrentWorkMessages(ARichEdit: TRichEdit;
          work_index: integer);
        procedure PrintDayLog(ARichEdit: TRichEdit; day, month, year: integer);
        procedure PrintWorkLog(ARichEdit: TRichEdit; record_id: longint);
    end;

var
    DataModule1: TDataModule1;

function ProductVarEqual(X, Y: RProductVar): boolean;

procedure PrintWorkMessages(ARichEdit: TRichEdit; work_index: integer;
  work: string; product_serial: variant; created_at: TDatetime; level: integer;
  Text: string);

implementation

uses dateutils, Vcl.dialogs, System.Variants, stringutils, variantutils,
    richeditutils;

{ %CLASSGROUP 'Vcl.Controls.TControl' }

{$R *.dfm}

function TReadVar.ProductVar: RProductVar;
begin
    result.FVar := FVar;
    result.FProduct := FProduct;
end;

function TReadVar.ValueError: RValueError;
begin
    if FError <> '' then
    begin
        result.FError := true;
        result.FValue := FError;
    end
    else
    begin
        result.FError := false;
        result.FValue := FloatToStr(FValue);
    end;
end;

function ProductVarEqual(X, Y: RProductVar): boolean;
begin
    result := (X.FProduct = Y.FProduct) AND (X.FVar = Y.FVar);
end;

constructor TProduct.Create(ASerial: integer; AChecked: boolean;
  AComport: string);
begin
    FSerial := ASerial;
    FChecked := AChecked;
    FComport := AComport;
end;

procedure TDataModule1.DataModuleCreate(Sender: TObject);
begin
    FDConnectionProductsDB.Connected := true;
    FDConnectionConfig.Connected := true;
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

function TDataModule1.ModbusCommands: TArray<TModbusCommand>;
var
    xs: TList<TModbusCommand>;
begin
    xs := TList<TModbusCommand>.Create;

    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text := 'SELECT * FROM command ORDER BY command_id;';
        Open;
        First;
        while not Eof do
        begin
            xs.Add(TModbusCommand.Create(FieldValues['command_id'],
              FieldValues['description']));
            Next;
        end;
        Free;
    end;
    result := xs.ToArray;
    xs.Free;

end;

function TDataModule1.InvertProductsChecked: boolean;
var
    count, I: integer;
    xs: TArray<TProduct>;

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

function TDataModule1.GetCurrentWorkCheckState(Ordinal: integer): string;
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
            result := FieldValues['checked'];
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

procedure TDataModule1.UpdateDeviceVarChecked(devicevar: integer;
  checked: boolean);
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text :=
          'UPDATE read_var SET checked = :checked WHERE read_var_id = :devicevar;';
        ParamByName('devicevar').Value := devicevar;
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

function TDataModule1.CurrentPartyID: int64;
begin
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text := 'select * from current_party_id';
        Open;
        result := FieldValues['party_id'];
        Free;
    end;
end;

function TDataModule1.CurrentPartyProducts: TArray<TProduct>;
var
    xs: TList<TProduct>;
    I, Serial: integer;

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

function TDataModule1.DeviceVars: TArray<TDeviceVar>;
var
    xs: TList<TDeviceVar>;
    v: TDeviceVar;
begin
    xs := TList<TDeviceVar>.Create;
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text := 'SELECT * FROM read_var ORDER BY read_var_id;';
        Open;
        First;
        while not Eof do
        begin
            v := TDeviceVar.Create;
            v.FVar := FieldValues['read_var_id'];
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

function TDataModule1.PartyValues(partyID: int64): TKeysValues;
var
    xs: TList<TKeyValue>;
begin
    xs := TList<TKeyValue>.Create;
    with TFDQuery.Create(nil) do
    begin
        Connection := FDConnectionProductsDB;
        SQL.Text :=
          'SELECT name, value FROM party_value2 WHERE party_id = :party_id;';
        ParamByName('party_id').Value := partyID;
        Open;
        First;
        while not Eof do
        begin
            xs.Add(TKeyValue.Create(FieldValues['name'], FieldValues['value']));
            Next;
        end;
        Close;
        Free;
    end;
    result := xs.ToArray;
    xs.Free;
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
    X: RProductCoefValue;
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
            X.Serial := FieldValues['product_serial'];
            X.Ordinal := FieldValues['ordinal'];
            X.Coef := FieldValues['coefficient_id'];
            X.Value := FieldValues['value'];
            xs.Add(X);
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

procedure PrintWorkMessages(ARichEdit: TRichEdit; work_index: integer;
  work: string; product_serial: variant; created_at: TDatetime; level: integer;
  Text: string);
var
    s: string;
begin
    s := Text;
    if not VariantIsEmptyOrNull(product_serial) then
        s := 'прибор ' + inttostr2(product_serial) + ': ' + s;
    if work <> '' then
        s := work + ': ' + s;
    if work_index <> 0 then
        s := '[' + inttostr2(work_index) + ']: ' + s;
    RichEdit_AddText(ARichEdit, IncHour(created_at, 3), level, s);
end;

procedure TDataModule1.PrintCurrentWorkMessages(ARichEdit: TRichEdit;
  work_index: integer);
var
    s: string;
begin
    ARichEdit.Lines.Clear;
    with FDQueryCurrentWorkMessages do
    begin
        ParamByName('work_index').Value := work_index;
        Open;
        First;
        while not Eof do
        begin
            PrintWorkMessages(ARichEdit, work_index, '',
              FieldValues['product_serial'],
              FieldValues['created_at'],
              FieldValues['level'],
              FieldValues['message']);
            Next;
        end;
        Close;
    end;
end;

procedure TDataModule1.PrintDayLog(ARichEdit: TRichEdit;
  day, month, year: integer);
begin
    ARichEdit.Lines.Clear;
    with FDQueryDayLog do
    begin
        ParamByName('day').Value := day;
        ParamByName('month').Value := month;
        ParamByName('year').Value := year;
        Open;
        First;
        while not Eof do
        begin
            PrintWorkMessages(ARichEdit, FieldValues['work_index'],
              FieldValues['work'], FieldValues['product_serial'],
              FieldValues['created_at'], FieldValues['level'],
              FieldValues['message']);
            Next;
        end;
        Close;
    end;
end;

procedure TDataModule1.PrintWorkLog(ARichEdit: TRichEdit; record_id: longint);
begin
    ARichEdit.Lines.Clear;
    with FDQueryWorkMessages do
    begin
        ParamByName('record_id').Value := record_id;
        Open;
        First;
        while not Eof do
        begin
            PrintWorkMessages(ARichEdit, FieldValues['work_index'],
              '', FieldValues['product_serial'],
              FieldValues['created_at'], FieldValues['level'],
              FieldValues['message']);
            Next;
        end;
        Close;
    end;
end;

end.
