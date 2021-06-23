FROM mcr.microsoft.com/dotnet/runtime:5.0 AS base
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:5.0 AS build
WORKDIR /src
COPY ["F_ACCORD_CORE/F_ACCORD_CORE.fsproj", "F_ACCORD_CORE/"]
RUN dotnet restore "F_ACCORD_CORE/F_ACCORD_CORE.fsproj"
COPY . .
WORKDIR "/src/F_ACCORD_CORE"
RUN dotnet build "F_ACCORD_CORE.fsproj" -c Release -o /app/build

FROM build AS publish
RUN dotnet publish "F_ACCORD_CORE.fsproj" -c Release -o /app/publish

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "F_ACCORD_CORE.dll"]
