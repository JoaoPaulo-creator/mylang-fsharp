build:
	dotnet build

clean:
	dotnet clean

raw:
	dotnet run file.my
run:
	./bin/Debug/net8.0/mylang-fsharp file.my
