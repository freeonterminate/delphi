unit uStyleUtils;

interface

procedure ClearStyles(const iObj: TObject);

implementation

uses
  System.Rtti, System.TypInfo, Vcl.Controls;

procedure ClearStylesSub(const iControl: TControl);
var
  i: Integer;
begin
  if (iControl <> nil) then begin
    if (iControl is TWinControl) then
      with TWinControl(iControl) do
        for i := 0 to ControlCount - 1 do
          ClearStylesSub(Controls[i]);

    iControl.StyleElements := [];
  end;
end;

procedure ClearStyles(const iObj: TObject);
var
  Context: TRttiContext;
  Fields: TArray<TRttiField>;
  Field: TRttiField;
  RttiType: TRttiType;
  Obj: TObject;
begin
  Context := TRttiContext.Create;
  try
    try
      Fields := Context.GetType(iObj.ClassType).GetFields;

      for Field in Fields do begin
        RttiType := Field.FieldType;

        if
          (RttiType = nil) or (RttiType.TypeKind <> tkClass) or
          (Field.Name = 'FOwner') or (Field.Name = 'FParent')
        then
          Continue;

        try
          Obj := Field.GetValue(iObj).AsObject;
        except
          Obj := nil;
        end;

        if (Obj <> nil) and (Obj is TControl) then
          ClearStylesSub(TControl(Obj));
      end;
    except
    end;
  finally
    Context.Free;
  end;
end;

end.
