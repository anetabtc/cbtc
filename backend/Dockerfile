####### DEPS #######
FROM node:18-alpine3.18 AS deps
RUN apk add --no-cache libc6-compat
WORKDIR /app

COPY package.json package-lock.json ./
RUN  npm install
RUN  npm install @mempool/mempool.js --save

####### BUILDER - node #######
FROM node:18-alpine3.18 AS builder
WORKDIR /app
COPY --from=deps /app/node_modules ./node_modules
COPY . .

#ENV NEXT_TELEMETRY_DISABLED 1


####### BUILDER2 - py ########
FROM python:3.11-alpine3.18 as builder2
WORKDIR /app

# install sys reqs *
RUN apk add --no-cache \
 linux-headers gcc g++ gfortran cython musl-dev gmp-dev libffi-dev libpq-dev openblas-dev 

# install deps into /app/.py directory
ADD ./requirements.txt /app/requirements.txt

# install deps into /app/.py directory
run pip install --target=/app/.py -r requirements.txt

####### RUNNER #######
FROM node:18-alpine3.18 AS runner
WORKDIR /app

### TODO: MOVE THIS TO ENV FILE... -_\

ENV VAULT_BTC_WALLET_ADDRESS="mwnCUR6TU9WRosy5gLFgNJMkssJ1XXHXpc"
ENV VAULT_BTC_WALLET_ID="AnetaTestnetBTCWallet3"
ENV VAULT_BTC_WALLET_MNEMONIC="notice carry sad utility gas spy vital yellow old test present vague"

# This is currently not being used for testing, but needs to be implemented for reliability / performance.
ENV DB_URI="postgresql://bitcoinlib:SUPERSECRETPASSHERE@172.17.0.1:5432/bitcoinlib"

ENV BLOCKFROST_KEY "preprodZ5sdrHtIciQKn3Ls3phRfCkqtVyFq5XX"
ENV API_URL "https://cardano-preprod.blockfrost.io/api/v0"
ENV NETWORK "Preprod"
ENV MINT_POLICY_ID = "2c04fa26b36a376440b0615a7cdf1a0c2df061df89c8c055e265050563425443"

###

RUN apk add --no-cache tzdata openblas gmp-dev python3=~3.11

ENV TZ="America/New_York"
RUN cp /usr/share/zoneinfo/America/New_York /etc/localtime
RUN echo "America/New_York" > /etc/timezone

ENV NODE_ENV production
#ENV NEXT_TELEMETRY_DISABLED 1

# Set PATH for Python libs / bin
ENV PATH=/app/.py/bin:$PATH
ENV PYTHONPATH="${PYTHONPATH}:/app/.py"

RUN addgroup --system --gid 1001 nodejs
RUN adduser --system --uid 1001 nextjs

# Install TypeScript stuff
RUN npm i ts-node
RUN npm i --save-dev @types/node
COPY --from=builder /app/server.ts ./server.ts


#COPY --from=builder --chown=nextjs:nodejs /app/.next ./.next
COPY --from=builder /app/node_modules ./node_modules
COPY --from=builder /app/package.json ./package.json
COPY --from=builder /app/tsconfig.json ./tsconfig.json
#COPY --from=builder /app/next.config.js ./next.config.js
#COPY --from=builder /app/next-env.d.ts ./next-env.d.ts
#COPY --from=builder /app/tailwind.config.js ./tailwind.config.js

COPY --from=builder /app/utils ./utils
COPY --from=builder /app/endpoints ./endpoints

# COPY --from=builder --chown=nextjs:nextjs /app/.next/standalone ./
# COPY --from=builder --chown=nextjs:nextjs /app/.next/static ./.next/static

COPY --from=builder /app/redeem.py ./redeem.py
COPY --from=builder2 /app/.py /app/.py

# USER nextjs

EXPOSE 3000

ENV PORT 3000

CMD ["npm", "run", "start"]
