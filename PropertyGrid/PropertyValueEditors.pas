unit PropertyValueEditors;

// Utility unit for the advanced Virtual Treeview demo application which contains the implementation of edit link
// interfaces used in other samples of the demo.

interface

uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    StdCtrls, VirtualTrees, ExtDlgs, ImgList, Buttons, ExtCtrls, ComCtrls,
    Mask, config;

type

    THandlePropertyValueChanged = reference to procedure(p:RConfigProperty);

    // ----------------------------------------------------------------------------------------------------------------------
    // Our own edit link to implement several different node editors.
    TPropertyEditLink = class(TInterfacedObject, IVTEditLink)
    private
        FEdit: TWinControl; // One of the property editor classes.
        FTree: TVirtualStringTree; // A back reference to the tree calling.
        FNode: PVirtualNode; // The node being edited.
        FColumn: integer; // The column of the node being edited.
        FHandlePropertyValueChanged : THandlePropertyValueChanged;
    protected
        procedure EditKeyDown(Sender: TObject; var Key: Word;
          Shift: TShiftState);
        procedure EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    public
        destructor Destroy; override;

        function BeginEdit: boolean; stdcall;
        function CancelEdit: boolean; stdcall;
        function EndEdit: boolean; stdcall;
        function GetBounds: TRect; stdcall;
        function PrepareEdit(Tree: TBaseVirtualTree; Node: PVirtualNode;
          Column: TColumnIndex): boolean; stdcall;
        procedure ProcessMessage(var Message: TMessage); stdcall;
        procedure SetBounds(R: TRect); stdcall;
        procedure SetPropertyValueChangedHandler(h: THandlePropertyValueChanged); stdcall;
    end;

    // ----------------------------------------------------------------------------------------------------------------------

type

    TPropertyTextKind = (ptkText, ptkHint);

    // The following constants provide the property tree with default data.



    // ----------------------------------------------------------------------------------------------------------------------

implementation

uses
    PropertyUtils, listports;

// ----------------- TPropertyEditLink ----------------------------------------------------------------------------------

// This implementation is used in VST3 to make a connection beween the tree
// and the actual edit window which might be a simple edit, a combobox
// or a memo etc.

destructor TPropertyEditLink.Destroy;

begin
    // FEdit.Free; casues issue #357. Fix:
    if FEdit.HandleAllocated then
        PostMessage(FEdit.Handle, CM_RELEASE, 0, 0);
    inherited;
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertyEditLink.SetPropertyValueChangedHandler(h: THandlePropertyValueChanged); stdcall;
begin
    FHandlePropertyValueChanged := h;
end;


procedure TPropertyEditLink.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);

var
    CanAdvance: boolean;

begin
    CanAdvance := true;

    case Key of
        VK_ESCAPE:
            begin
                Key := 0; // ESC will be handled in EditKeyUp()
            end;
        VK_RETURN:
            if CanAdvance then
            begin
                FTree.EndEditNode;
                Key := 0;
            end;

        VK_UP, VK_DOWN:
            begin
                // Consider special cases before finishing edit mode.
                CanAdvance := Shift = [];
                if FEdit is TComboBox then
                    CanAdvance := CanAdvance and not TComboBox(FEdit)
                      .DroppedDown;
                if FEdit is TDateTimePicker then
                    CanAdvance := CanAdvance and not TDateTimePicker(FEdit)
                      .DroppedDown;

                if CanAdvance then
                begin
                    // Forward the keypress to the tree. It will asynchronously change the focused node.
                    PostMessage(FTree.Handle, WM_KEYDOWN, Key, 0);
                    Key := 0;
                end;
            end;
    end;
end;

procedure TPropertyEditLink.EditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
    case Key of
        VK_ESCAPE:
            begin
                FTree.CancelEditNode;
                Key := 0;
            end; // VK_ESCAPE
    end; // case
end;

// ----------------------------------------------------------------------------------------------------------------------

function TPropertyEditLink.BeginEdit: boolean;

begin
    Result := true;
    FEdit.Show;
    FEdit.SetFocus;
end;

// ----------------------------------------------------------------------------------------------------------------------

function TPropertyEditLink.CancelEdit: boolean;

begin
    Result := true;
    FEdit.Hide;
end;

// ----------------------------------------------------------------------------------------------------------------------

function TPropertyEditLink.EndEdit: boolean;

var
    Data: PConfigProperty;
    Buffer: array [0 .. 1024] of Char;
    S: UnicodeString;

begin
    Result := true;

    Data := FTree.GetNodeData(FNode);
    if FEdit is TComboBox then
        S := TComboBox(FEdit).Text
    else if FEdit is TCheckBox then
    begin
        if (FEdit as TCheckBox).Checked then
            S := '1'
        else
            s := '0';

    end else
    begin
        GetWindowText(FEdit.Handle, Buffer, 1024);
        S := Buffer;
    end;

    if S <> Data.FValue then
    begin
        Data.SetStr(S);
        if Assigned(FHandlePropertyValueChanged) then
            FHandlePropertyValueChanged(Data^);

        //DataModule1.UpdateConfigPropertyValue(Data.FSectionName, Data.FPropertyName, Data.FValue);
        FTree.InvalidateNode(FNode);
        FTree.InvalidateNode(FNode.Parent);
    end;
    FEdit.Hide;
    FTree.SetFocus;
end;

// ----------------------------------------------------------------------------------------------------------------------

function TPropertyEditLink.GetBounds: TRect;

begin
    Result := FEdit.BoundsRect;
end;

// ----------------------------------------------------------------------------------------------------------------------

function TPropertyEditLink.PrepareEdit(Tree: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex): boolean;

var
    Data: PConfigProperty;
    i: integer;

begin
    Result := true;
    FTree := Tree as TVirtualStringTree;
    FNode := Node;
    FColumn := Column;

    // determine what edit type actually is needed
    FEdit.Free;
    FEdit := nil;
    Data := FTree.GetNodeData(Node);

    if (length(Data.FList) > 0) or (Data.FType = VtcComportName) or
      (Data.FType = VtcBaud) then
    begin
        FEdit := TComboBox.Create(nil);
        with FEdit as TComboBox do
        begin
            Visible := False;
            Parent := Tree;
            Text := Data.FValue;
            if Data.FType = VtcComportName then
                EnumComPorts(Items)
            else if Data.FType = VtcBaud then
            begin
                Items.Add('1200');
                Items.Add('2400');
                Items.Add('4800');
                Items.Add('9600');
                Items.Add('14400');
                Items.Add('19200');
                Items.Add('38400');
                Items.Add('56000');
                Items.Add('57600');
                Items.Add('115200');
                Items.Add('128000');
                Items.Add('256000');
            end

            else
                for i := 0 to length(Data.FList) - 1 do
                    Items.Add(Data.FList[i]);
            OnKeyDown := EditKeyDown;
            OnKeyUp := EditKeyUp;
            style := csDropDownList;
            ItemHeight := 22;
            ItemIndex := Items.IndexOf(Data.FValue);
        end;
    end

    else if Data.FType = VtcString then
    begin
        FEdit := TEdit.Create(nil);
        with FEdit as TEdit do
        begin
            Visible := False;
            Parent := Tree;
            Text := Data.FValue;
            OnKeyDown := EditKeyDown;
            OnKeyUp := EditKeyUp;
        end;
    end

    else if Data.FType = VtcInt then
    begin
        FEdit := TEdit.Create(nil);
        with FEdit as TEdit do
        begin
            Visible := False;
            Parent := Tree;
            Text := Data.FValue;
            OnKeyDown := EditKeyDown;
            OnKeyUp := EditKeyUp;
        end;
    end
    else if Data.FType = VtcFloat then
    begin
        FEdit := TEdit.Create(nil);
        with FEdit as TEdit do
        begin
            Visible := False;
            Parent := Tree;
            Text := Data.FValue;
            OnKeyDown := EditKeyDown;
            OnKeyUp := EditKeyUp;
        end;
    end

    else if Data.FType = VtcBool then
    begin
        FEdit := TCheckBox.Create(nil);
        with FEdit as TCheckBox do
        begin
            Visible := False;
            Parent := Tree;
            if Data.FValue = '0' then
                Data.FValue := '1'
            else
                Data.FValue := '0';

            Checked := Data.FValue <> '0';
            Caption := '---';

            Data.SetStr(Data.FValue);
            if Assigned(FHandlePropertyValueChanged) then
                FHandlePropertyValueChanged(Data^);

        end;
    end
    else
        Result := False;
end;


// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertyEditLink.ProcessMessage(var Message: TMessage);

begin
    FEdit.WindowProc(Message);
end;

// ----------------------------------------------------------------------------------------------------------------------

procedure TPropertyEditLink.SetBounds(R: TRect);

var
    Dummy: integer;

begin
    // Since we don't want to activate grid extensions in the tree (this would influence how the selection is drawn)
    // we have to set the edit's width explicitly to the width of the column.
    FTree.Header.Columns.GetColumnBounds(FColumn, Dummy, R.Right);
    FEdit.BoundsRect := R;
end;
// ----------------------------------------------------------------------------------------------------------------------

end.
