# Stage 1: build
FROM registry.gitlab.com/natsukagami/docker-fsharp:netcore

WORKDIR /app

COPY . .

RUN sed -i 's/<Import Project="fsc.props" \/>//' Narumi.Bot/Narumi.Bot.fsproj
# RUN dotnet build Narumi.Bot
RUN dotnet publish Narumi.Bot -c Release -r linux-x64 --self-contained true 

# Stage 2: publish
FROM registry.gitlab.com/natsukagami/docker-fsharp:netcore

WORKDIR /

COPY --from=0 /app/Narumi.Bot/bin/Release/netcoreapp2.2/linux-x64/publish /app

WORKDIR /app

CMD ["./Narumi.Bot"]