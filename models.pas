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

    RProductVarOrder = record
        FProductOrder: integer;
        FVarOrder: integer;
    end;

    RProductCoefOrder = record
        FProductOrder: integer;
        FCoefOrder: integer;
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

    TProductVarValues = TDictionary<RProductVarOrder, RValueError>;

    TReadVar = class
        FProductOrder: integer;
        FProductSerial: integer;
        FVarOrder: integer;
        FVar: integer;
        FVarName: string;
        FValue: double;
        FError: string;
        function ProductVarOrder: RProductVarOrder;
        function ValueError: RValueError;
    end;

    TReadCoef = class
        FProductOrder: integer;
        FProductSerial: integer;
        FCoefficientOrder: integer;
        FCoefficient: integer;
        FCoefficientName: string;
        FValue: double;
        FError: string;
        function ProductCoefOrder: RProductCoefOrder;
        function ValueError: RValueError;
    end;

function ProductVarEqual(X, Y: RProductVarOrder): boolean;
function ProductCoefEqual(X, Y: RProductCoefOrder): boolean;

implementation

uses stringutils, sysutils;

function TReadVar.ProductVarOrder: RProductVarOrder;
begin
    result.FVarOrder := FVarOrder;
    result.FProductOrder := FProductOrder;
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

function TReadCoef.ProductCoefOrder: RProductCoefOrder;
begin
    result.FCoefOrder := FCoefficientOrder;
    result.FProductOrder := FProductOrder;
end;

function TReadCoef.ValueError: RValueError;
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

function ProductVarEqual(X, Y: RProductVarOrder): boolean;
begin
    result := (X.FProductOrder = Y.FProductOrder) AND (X.FVarOrder = Y.FVarOrder);
end;

function ProductCoefEqual(X, Y: RProductCoefOrder): boolean;
begin
    result := (X.FProductOrder = Y.FProductOrder) AND (X.FCoefOrder = Y.FCoefOrder);
end;

constructor TProduct.Create(ASerial: integer; AChecked: boolean;
  AComport: string);
begin
    FSerial := ASerial;
    FChecked := AChecked;
    FComport := AComport;
end;

end.
