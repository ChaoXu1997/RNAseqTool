#!/usr/bin/env bash
# RNAseqTool 开发环境一键启动脚本
# 用法: ./dev.sh [--backend|--frontend|--stop]

set -euo pipefail

ROOT_DIR="$(cd "$(dirname "$0")" && pwd)"
BACKEND_DIR="$ROOT_DIR/backend"
FRONTEND_DIR="$ROOT_DIR/frontend"
PID_DIR="$ROOT_DIR/.dev-pids"
LOG_DIR="$ROOT_DIR/.dev-logs"

BACKEND_PORT=8000
FRONTEND_PORT=3000

# 颜色
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
CYAN='\033[0;36m'
NC='\033[0m'

log()  { echo -e "${GREEN}[RNAseqTool]${NC} $*"; }
warn() { echo -e "${YELLOW}[RNAseqTool]${NC} $*"; }
err()  { echo -e "${RED}[RNAseqTool]${NC} $*" >&2; }

mkdir -p "$PID_DIR" "$LOG_DIR"

# 检查端口是否已被占用，是则返回 PID
port_pid() {
    lsof -ti ":$1" 2>/dev/null || true
}

stop_service() {
    local name=$1 port=$2 pidfile="$PID_DIR/$1.pid"
    local pid
    pid=$(port_pid "$port")
    if [ -n "$pid" ]; then
        log "停止 $name (PID: $pid, port: $port)..."
        kill "$pid" 2>/dev/null || true
        sleep 1
        # 强制杀
        pid=$(port_pid "$port")
        [ -n "$pid" ] && kill -9 "$pid" 2>/dev/null || true
        log "$name 已停止"
    else
        warn "$name 未在运行"
    fi
    rm -f "$pidfile"
}

stop_all() {
    stop_service "backend" "$BACKEND_PORT"
    stop_service "frontend" "$FRONTEND_PORT"
    log "全部服务已停止"
}

start_backend() {
    local pid
    pid=$(port_pid "$BACKEND_PORT")
    if [ -n "$pid" ]; then
        warn "后端已在运行 (PID: $pid, port: $BACKEND_PORT)"
        return 0
    fi

    # 检查 R
    if ! command -v Rscript &>/dev/null; then
        err "未找到 Rscript，请先安装 R"
        return 1
    fi

    log "启动后端 (port: $BACKEND_PORT)..."
    cd "$BACKEND_DIR"
    nohup Rscript run.R > "$LOG_DIR/backend.log" 2>&1 &
    local rpid=$!
    echo "$rpid" > "$PID_DIR/backend.pid"
    cd "$ROOT_DIR"

    # 等待就绪
    local i=0
    while [ $i -lt 30 ]; do
        sleep 1
        if curl -sf "http://localhost:$BACKEND_PORT/api/health" >/dev/null 2>&1; then
            log "后端就绪 (PID: $rpid)"
            return 0
        fi
        i=$((i + 1))
    done

    err "后端启动超时，查看日志: $LOG_DIR/backend.log"
    return 1
}

start_frontend() {
    local pid
    pid=$(port_pid "$FRONTEND_PORT")
    if [ -n "$pid" ]; then
        warn "前端已在运行 (PID: $pid, port: $FRONTEND_PORT)"
        return 0
    fi

    # 检查 node_modules
    if [ ! -d "$FRONTEND_DIR/node_modules" ]; then
        log "安装前端依赖..."
        cd "$FRONTEND_DIR" && npm install && cd "$ROOT_DIR"
    fi

    log "启动前端 (port: $FRONTEND_PORT)..."
    cd "$FRONTEND_DIR"
    nohup npx vite --port "$FRONTEND_PORT" > "$LOG_DIR/frontend.log" 2>&1 &
    local fpid=$!
    echo "$fpid" > "$PID_DIR/frontend.pid"
    cd "$ROOT_DIR"

    # 等待就绪
    local i=0
    while [ $i -lt 20 ]; do
        sleep 1
        if curl -sf "http://localhost:$FRONTEND_PORT" >/dev/null 2>&1; then
            log "前端就绪 (PID: $fpid)"
            return 0
        fi
        i=$((i + 1))
    done

    err "前端启动超时，查看日志: $LOG_DIR/frontend.log"
    return 1
}

show_status() {
    echo ""
    echo -e "${CYAN}═══════════════════════════════════════${NC}"
    echo -e "${CYAN}  RNAseqTool 开发服务器状态${NC}"
    echo -e "${CYAN}═══════════════════════════════════════${NC}"

    local bp fp
    bp=$(port_pid "$BACKEND_PORT")
    fp=$(port_pid "$FRONTEND_PORT")

    if [ -n "$bp" ]; then
        echo -e "  后端 (R plumber):  ${GREEN}运行中${NC}  http://localhost:$BACKEND_PORT"
    else
        echo -e "  后端 (R plumber):  ${RED}未启动${NC}"
    fi

    if [ -n "$fp" ]; then
        echo -e "  前端 (Vite dev):   ${GREEN}运行中${NC}  http://localhost:$FRONTEND_PORT"
    else
        echo -e "  前端 (Vite dev):   ${RED}未启动${NC}"
    fi

    echo -e "${CYAN}═══════════════════════════════════════${NC}"
    echo ""
}

# ─── 主逻辑 ───

case "${1:-}" in
    --stop)
        stop_all
        ;;
    --backend)
        start_backend
        show_status
        ;;
    --frontend)
        start_frontend
        show_status
        ;;
    --status)
        show_status
        ;;
    --logs)
        tail -f "$LOG_DIR/backend.log" "$LOG_DIR/frontend.log"
        ;;
    *)
        # 默认：启动全部
        start_backend
        start_frontend
        show_status
        log "浏览器打开: http://localhost:$FRONTEND_PORT"
        log "停止服务:   ./dev.sh --stop"
        log "查看日志:   ./dev.sh --logs"
        ;;
esac
