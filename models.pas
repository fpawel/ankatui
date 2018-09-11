unit models;

interface

uses System.Generics.collections;

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
        FProductOrder: integer;
        FVarOrder: integer;
        FValue: double;
        FError: string;
        function ProductVar: RProductVar;
        function ValueError: RValueError;
    end;

function ProductVarEqual(X, Y: RProductVar): boolean;

implementation

uses stringutils, sysutils;

function TReadVar.ProductVar: RProductVar;
begin
    result.FVar := FVarOrder;
    result.FProduct := FProductOrder;
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
        result.FValue := floattostr(FValue);
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

end.
