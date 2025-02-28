#!/usr/bin/env bash
# -*- coding: utf-8 -*-
# shellcheck shell=bash disable=SC1091,SC2039,SC2166
#
#  fr.sh
#  Created: 2019/12/13 - 00:00
#  Altered: 2024/10/03 - 00:00
#  Updated: 2025/01/08 - 00:44
#
#  Copyright (c) 2019-2025, Vilmar Catafesta <vcatafesta@gmail.com>
#  All rights reserved.
#
#  Redistribution and use in source and binary forms, with or without
#  modification, are permitted provided that the following conditions
#  are met:
#  1. Redistributions of source code must retain the above copyright
#     notice, this list of conditions and the following disclaimer.
#  2. Redistributions in binary form must reproduce the above copyright
#     notice, this list of conditions and the following disclaimer in the
#     documentation and/or other materials provided with the distribution.
#
#  THIS SOFTWARE IS PROVIDED BY THE AUTHOR AS IS'' AND ANY EXPRESS OR
#  IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
#  OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
#  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
#  INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
#  NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
#  THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
##############################################################################
#export LANGUAGE=pt_BR
export TEXTDOMAINDIR=/usr/share/locale
export TEXTDOMAIN=fr.sh

export TERM=${TERM:-xterm}
export TERM=${TERM:-xterm-256color}

red="\033[01;31m"
green="\033[01;32m"
yellow="\033[01;33m"
blue="\033[01;34m"
pink="\033[01;35m"
cyan="\033[01;36m"
reset="\033[0m"

die() {
	local msg="$1"
  export TERM=${TERM:-xterm}
  export TERM=${TERM:-xterm-256color}
  #Definindo variáveis de cores
	msg="$(sed 's/<[^>]*>//g' <<<"$msg")" # Remove as tags HTML
	echo -e "BP=>${cyan}error: ${red}${msg}${reset}"
	exit 1
}
export -f die

msg_raw() {
	local msg="$1"
	# Remove tags HTML, se existirem
	#msg="$(sed 's/<[^>]*>//g' <<< "$msg")"

	# Verifica se existe ':' na mensagem
	if [[ "$msg" == *:* ]]; then
		# Divide a string antes e depois do primeiro ':'
		local before_colon="${msg%%:*}:"
		local after_colon="${msg#*: }"
		# Aplica as cores
		msg="${cyan}${before_colon} ${red}${after_colon}${reset}"
	else
		# Se não houver ':', aplica apenas a cor padrão
		msg="${cyan}${msg}${reset}"
	fi
	echo -e "$msg"
}
export -f msg_raw

msg() {
	local msg="$1"
	msg="$(sed 's/<[^>]*>//g' <<<"$msg")" # Remove as tags HTML
	echo -e "BP=>${cyan}running: ${yellow}${msg}${reset}"
}
export -f msg

msg_ok() {
	local msg="$1"
	msg="$(sed 's/<[^>]*>//g' <<<"$msg")" # Remove as tags HTML
	echo -e "BP=>${cyan}feito: ${green}${msg}${reset}"
}
export -f msg_ok

msg_run() {
	local msg="$1"
	echo -e "BP=>${cyan}running: ${yellow}${msg}${reset}"
	eval "$msg"
}
export -f msg_run

msg_info() {
  local msg="$1"
  local caller_function="${FUNCNAME[1]}"      # Nome da função que chamou a função atual
  local caller_line="${BASH_LINENO[1]}"       # Número da linha que chamou a função atual
  msg="$(sed 's/<[^>]*>//g' <<<"$msg")"       # Remove as tags HTML
  #echo -e "${blue}==>${green}[${caller_function}:${caller_line}]=>${yellow}info   : ${cyan}${msg}${reset}"
  echo -e "${caller_function}=>${yellow}info   : ${cyan}${msg}${reset}"
}
export -f msg_info

msg_warning() {
	local msg="$1"
  local caller_function="${FUNCNAME[1]}"      # Nome da função que chamou a função atual
  local caller_line="${BASH_LINENO[1]}"       # Número da linha que chamou a função atual
	msg="$(sed 's/<[^>]*>//g' <<<"$msg")" # Remove as tags HTML
  echo -e "${caller_function}=>${red}warning: ${orange}${msg}${reset}"
}
export -f msg_warning

msg_warn() {
	local msg="$1"
  local caller_function="${FUNCNAME[1]}"      # Nome da função que chamou a função atual
  local caller_line="${BASH_LINENO[1]}"       # Número da linha que chamou a função atual
	msg="$(sed 's/<[^>]*>//g' <<<"$msg")" # Remove as tags HTML
  echo -e "${caller_function}=>${red}warning: ${orange}${msg}${reset}"
}
export -f msg_warn

replicate() {
	local char=${1:-'#'}
	local nsize=${2:-$(tput cols)}
	local line
	printf -v line "%*s" "$nsize" && echo -e "${blue}${line// /$char}${reset}"
}
export -f replicate

send_telegram_message() {
	local message="$1"
	local parse_mode="$2"

	# Define parse_mode como "MarkdownV2" se não for especificado
	[[ -z $parse_mode ]] && parse_mode="HTML"

	# Remove as tags HTML e exibe o resultado no terminal
	echo -e "${red}$(sed 's/<[^>]*>//g' <<<"$message")${reset}"
	# Envia a mensagem original com HTML para o Telegram
	curl -s -X POST "https://api.telegram.org/bot${inputs_telegram_token}/sendMessage" \
		-d chat_id="${inputs_telegram_chat_id}" \
		-d text="$message" \
		-d parse_mode="$parse_mode"
}
export -f send_telegram_message

supports_uefi() {
	fdisk -l "$1" 2>/dev/null | grep -q "EFI"
}

detect_audio_server() {
	if pgrep -x pipewire >/dev/null; then
		echo "pipewire"
	elif pgrep -x pulseaudio >/dev/null; then
		echo "pa"
	elif pgrep -x jackd >/dev/null; then
		echo "jack"
	else
		echo "none"
	fi
}

fr() {
	local force_uefi=false
	local spice=false
	local drive=false
	local internal=true
	local xmem=8G
	local img=""

	# Processar argumentos
	while [[ $# -gt 0 ]]; do
		case "$1" in
		-x|--internal)
			internal=false
			shift
			;;
		-s|--spice)
			spice=true
			shift
			;;
		-u|--force-uefi)
			force_uefi=true
			shift
			;;
		-m|--memory)
			xmem="$2"
			shift
			shift
			;;
		-d|--drive)
			drive_path="$2"
			if ! lsblk $drive_path &>/dev/null; then
			  msg_warn "$drive_path não é um dispositivo de bloco"
			  return 1
			fi
			drive=true
			shift
			shift
			;;
		-*)
			msg_warn "Erro: Opção desconhecida: $1"
			return 1
			;;
		*)
			img="$1"
			shift
			;;
		esac
	done

	# Verificar se a imagem foi especificada
	[[ -z "$img" ]] && {
		msg_warn "Erro: Imagem/Device não especificada!"
		return 1
	}
	[[ ! -e "$img" ]] && {
		msg_warn "Erro: Imagem/Device ${img} não encontrada!"
		return 1
	}

  # Forçar UEFI, se solicitado
  if $force_uefi; then
    msg_info "Forçando inicialização UEFI..."
    qemu_options+=(-drive if=pflash,format=raw,readonly=on,file=/usr/share/edk2/x64/OVMF.4m.fd)
  else
    msg_warn "Parametro -u não informado, usando BIOS Legacy (padrão)..."
  fi

	# Adicionar opções padrão do QEMU
	declare -a qemu_options=()
	qemu_options+=(-no-fd-bootchk)
	qemu_options+=(-machine accel=kvm)
	qemu_options+=(-cpu host)
	qemu_options+=(-smp "$(nproc)")
	qemu_options+=(-m $xmem)
	qemu_options+=(-k pt-br)

	qemu_options+=(-drive file=${img},if=none,id=disk1,format=raw)
	qemu_options+=(-device ide-hd,drive=disk1,bootindex=1)

  if $drive; then
	msg_info "Anexando hd externo $drive_path"
  qemu_options+=(-drive file=$drive_path,format=raw,media=disk)
	fi

  if $internal; then
	msg_info "Anexando hd /archlive/qemu/hda.img"
	qemu_options+=(-drive file=/archlive/qemu/hda.img,format=raw)
	fi

	qemu_options+=(-name "fr $*",process=archiso_0)
	qemu_options+=(-device virtio-scsi-pci,id=scsi0)
	qemu_options+=(-audiodev "$(detect_audio_server)",id=snd0)
	qemu_options+=(-rtc base=localtime,clock=host)
	qemu_options+=(-device ich9-intel-hda)
	qemu_options+=(-device hda-output,audiodev=snd0)
	qemu_options+=(-global ICH9-LPC.disable_s3=1)
	qemu_options+=(-machine type=q35,smm=on,accel=kvm,usb=on,pcspk-audiodev=snd0)

	# Configurar SPICE e rede
	qemu_options+=(-netdev user,id=net0)
	qemu_options+=(-device e1000,netdev=net0)
	qemu_options+=(-device virtio-serial)

	if $spice; then
  	# Informar as portas usadas
  	local random_port="$(shuf -i 4444-45000 -n 1)"
  	local random_spice_port="$(shuf -i 5900-5910 -n 1)"
  	msg_info "spice running on port: $random_spice_port"
	  msg_info "remote-viewer spice://localhost:$random_spice_port"

  	qemu_options+=(-spice port=$random_spice_port,disable-ticketing=on)
	  qemu_options+=(-monitor tcp:localhost:$random_port,server,nowait)
  	qemu_options+=(-chardev spicevmc,id=vdagent,debug=0,name=vdagent)
	  qemu_options+=(-device virtserialport,chardev=vdagent,name=com.redhat.spice.0)

		# Executar o QEMU
		sudo env XDG_RUNTIME_DIR=/run/user/$(id -u) qemu-system-x86_64 "${qemu_options[@]}" 2>/dev/null &
		qemu_pid=$! 2>/dev/null
		remote-viewer spice://localhost:$random_spice_port &
		viewer_pid=$! 2>/dev/null

		# Aguardar o remote-viewer e encerrar o QEMU quando ele fechar
		wait $viewer_pid
		kill $qemu_pid 2>/dev/null
  else
		# Executar o QEMU
		sudo env XDG_RUNTIME_DIR=/run/user/$(id -u) qemu-system-x86_64 "${qemu_options[@]}" 2>/dev/null
  fi
}
export -f fr
