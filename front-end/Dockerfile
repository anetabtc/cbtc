####### DEPS #######
FROM node:18-alpine AS deps
RUN apk add --no-cache libc6-compat
WORKDIR /app

COPY package.json package-lock.json ./
RUN  npm install --production

####### BUILDER #######
FROM nikolaik/python-nodejs:python3.10-nodejs18-alpine AS builder
WORKDIR /app
COPY --from=deps /app/node_modules ./node_modules
COPY . .

ENV NEXT_TELEMETRY_DISABLED 1

RUN npm run build

# TODO: All Python code needs to go to another container / VM. For testing, okay i guess...

# add python reqs
ADD ./requirements.txt /app/requirements.txt

# install sys reqs *** TODO: Review what is not needed ***
RUN apk add --no-cache \
 make automake gcc g++ linux-headers subversion python3-dev gcc libc-dev gmp-dev libffi-dev tzdata libpq-dev python3-dev

# install deps into /app/.py directory
run pip install --target=/app/.py -r requirements.txt

####### RUNNER #######
FROM nikolaik/python-nodejs:python3.10-nodejs18-alpine AS runner
WORKDIR /app

### TODO: MOVE THIS TO ENV FILE... -_\

ENV VAULT_BTC_WALLET_ADDRESS="n4YDfMoo1i3rzF8XEq9zyfo8TFfnroLjy6"
ENV VAULT_BTC_WALLET_ID="AnetaTestnetBTCWallet2"
ENV VAULT_BTC_WALLET_MNEMONIC="tray bulk patrol supreme talent various tiny hybrid hawk armed grain pitch"

# This is currently not being used for testing, but needs to be implemented for reliability / performance.
ENV DB_URI="postgresql://bitcoinlib:SUPERSECRETPASSHERE@172.17.0.1:5432/bitcoinlib"

ENV BLOCKFROST_KEY "preprodK6mCcxenBR5mgRYlRZzFA7NlKoKUR1AQ"
ENV API_URL "https://cardano-preprod.blockfrost.io/api/v0"
ENV NETWORK "Preprod"

###

RUN apk add --no-cache tzdata gmp-dev

ENV TZ="America/New_York"
RUN cp /usr/share/zoneinfo/America/New_York /etc/localtime
RUN echo "America/New_York" > /etc/timezone

ENV NODE_ENV production
ENV NEXT_TELEMETRY_DISABLED 1

# Set PATH for Python libs / bin
ENV PATH=/app/.py/bin:$PATH
ENV PYTHONPATH="${PYTHONPATH}:/app/.py"

RUN addgroup --system --gid 1001 nodejs
RUN adduser --system --uid 1001 nextjs

COPY --from=builder --chown=nextjs:nodejs /app/.next ./.next
COPY --from=builder /app/node_modules ./node_modules
COPY --from=builder /app/package.json ./package.json
COPY --from=builder /app/tsconfig.json ./tsconfig.json
COPY --from=builder /app/next.config.js ./next.config.js
COPY --from=builder /app/next-env.d.ts ./next-env.d.ts
COPY --from=builder /app/tailwind.config.js ./tailwind.config.js
COPY --from=builder /app/postcss.config.js ./postcss.config.js

COPY --from=builder /app/public ./public
COPY --from=builder /app/utils ./utils
COPY --from=builder /app/pages ./pages
COPY --from=builder /app/endpoints ./endpoints
COPY --from=builder /app/components ./components
COPY --from=builder /app/styles ./styles

COPY --from=builder --chown=nextjs:nextjs /app/.next/standalone ./
COPY --from=builder --chown=nextjs:nextjs /app/.next/static ./.next/static

COPY --from=builder /app/.py /app/.py
COPY --from=builder /app/hello.py ./hello.py

USER nextjs

EXPOSE 3000

ENV PORT 3000

CMD ["npm", "start"]
