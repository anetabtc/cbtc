FROM node:18-alpine AS deps
RUN apk add --no-cache libc6-compat
WORKDIR /app

COPY package.json package-lock.json ./
RUN  npm install --production

FROM node:18-alpine AS builder
WORKDIR /app
COPY --from=deps /app/node_modules ./node_modules
COPY . .

ENV NEXT_TELEMETRY_DISABLED 1

RUN npm run build

FROM nikolaik/python-nodejs:python3.10-nodejs18-alpine AS runner
WORKDIR /app

###

# add requirements (to leverage Docker cache)
ADD ./requirements.txt /app/requirements.txt

# install requirements
RUN apk add --update-cache \
 make automake gcc g++ linux-headers subversion python3-dev gcc libc-dev gmp-dev libffi-dev tzdata libpq-dev python3-dev
ENV TZ="America/New_York"
RUN cp /usr/share/zoneinfo/America/New_York /etc/localtime

RUN export LDFLAGS="-L/usr/local/opt/openssl/lib -L /usr/local/opt/gmp/lib"
RUN export CPPFLAGS="-I/usr/local/opt/openssl/include -I/usr/local/opt/gmp/include"

RUN pip install --upgrade pip
RUN pip install -r requirements.txt

ARG SCALA_VERSION=2.12.11
ARG SBT_VERSION=1.3.9
ENV SCALA_HOME=/usr/share/scala

RUN apk add openjdk11-jre
COPY . /usr/src/app

# IMPORTANT PARAMETERS

ENV VAULT_BTC_WALLET_ADDRESS="n4YDfMoo1i3rzF8XEq9zyfo8TFfnroLjy6"
ENV VAULT_BTC_WALLET_ID="AnetaTestnetBTCWallet2"
ENV VAULT_BTC_WALLET_MNEMONIC="tray bulk patrol supreme talent various tiny hybrid hawk armed grain pitch"

ENV DB_URI="postgresql://bitcoinlib:T3mp2232@172.17.0.1:5432/bitcoinlib"

###

ENV NODE_ENV production
ENV NEXT_TELEMETRY_DISABLED 1

RUN addgroup --system --gid 1001 nodejs
RUN adduser --system --uid 1001 nextjs

COPY --from=builder --chown=nextjs:nodejs /app/.next ./.next
COPY --from=builder /app/node_modules ./node_modules
COPY --from=builder /app/package.json ./package.json

USER nextjs

EXPOSE 3000

ENV PORT 3000

CMD ["npm", "start"]

ENV BLOCKFROST_KEY "preprodK6mCcxenBR5mgRYlRZzFA7NlKoKUR1AQ"
ENV API_URL "https://cardano-preprod.blockfrost.io/api/v0"
ENV NETWORK "Preprod"
