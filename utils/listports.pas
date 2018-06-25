unit listports;

interface

uses classes;

procedure EnumComPorts(const Ports: TStrings);

implementation

uses registry, windows;

procedure EnumComPorts(const Ports: TStrings);

var
    nInd: Integer;

begin { EnumComPorts }
    with TRegistry.Create(KEY_READ) do
        try
            RootKey := HKEY_LOCAL_MACHINE;
            if OpenKey('hardware\devicemap\serialcomm', False) then
                try
                    Ports.BeginUpdate();
                    try
                        GetValueNames(Ports);
                        for nInd := Ports.Count - 1 downto 0 do
                            Ports.Strings[nInd] :=
                              ReadString(Ports.Strings[nInd]);

                    finally
                        Ports.EndUpdate()
                    end { try-finally }
                finally
                    CloseKey()
                end { try-finally }
            else
                Ports.Clear()
        finally
            Free()
        end { try-finally }
end { EnumComPorts };

end.
