unit variantutils;

interface

uses System.Variants;

function VariantIsEmptyOrNull(const Value: Variant): Boolean;

implementation

function VariantIsEmptyOrNull(const Value: Variant): Boolean;
begin
  Result := VarIsClear(Value) or VarIsEmpty(Value) or VarIsNull(Value) or (VarCompareValue(Value, Unassigned) = vrEqual);
  if (not Result) and VarIsStr(Value) then
    Result := Value = '';
end;

end.
