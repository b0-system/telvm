(*---------------------------------------------------------------------------
   Copyright (c) 2025 The telvm programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

(** QEMU invocation logic fragments. *)

open B0_std

(** {1:base Base parameters} *)

val guest_archs : Os.Arch.t list
(** [guest_archs] are the guest archs supported by [telvm]. *)

val system : guest_arch:Os.Arch.t -> (Cmd.t, string) result
(** [system] determines the QEMU executable to run. *)

val system_version : Cmd.t -> (string option, string) result
(** [system_version cmd] is the QEMU version of the system
    [cmd] or [None] if it cannot be found in the [PATH]. *)

val machine : guest_arch:Os.Arch.t -> (string, string) result
(** [machine] determines the value of the [-machine] option. *)

val accel :
  host_os:Os.Name.t -> host_arch:Os.Arch.t -> guest_arch:Os.Arch.t ->
  accel:string option -> string

(** [accel] determines the value of the [-accel] option. *)

val cpu :
  host_arch:Os.Arch.t -> guest_arch:Os.Arch.t -> (string, string) result
(** [cpu] determines the value of the [-cpu] option. *)

val smp : string option -> string
(** [smp] dtermines the value of the [-smp] option. Defaults to ["4"]. *)

val mem : string option -> string
(** [mem] dtermines the value of the [-mem] option. Defaults to ["4G"]. *)

val base_cmd :
  system:Cmd.t -> machine:string -> accel:string -> cpu:string ->
  smp:string -> mem:string -> Cmd.t
(** [base_cmd] is the [system] executable with corresponding parameters *)

val auto_base_cmd :
  host_os:Os.Name.t -> host_arch:Os.Arch.t -> guest_arch:Os.Arch.t ->
  accel:string option -> smp:string option -> mem:string option ->
  (Cmd.t, string) result

(** {1:bootloader Bootloader parameters} *)

val uefi_code :
  host_os:Os.Name.t -> guest_arch:Os.Arch.t -> (Fpath.t * string, string) result
(** [uefi_code] determines the file where the UEFI blob stored and its
    format *)

val uefi_drive : uefi_code:(Fpath.t * string) -> Cmd.t
(** [uefi_drive] is a [-drive] option with [uefi_code] as read-only flash
    memory with given format.  *)

(** {1:networking Networking parameters} *)

val nic_virtio_net_pci : string

(** {1:usb USB devices}

    See also {!section-drives}. *)

val device_usb_qemu_xhci : unit -> Cmd.t
(** [device_usb_qemu_xhci] is the [-device qemu-xhci].  This gives the
    machine an USB xHCI controller. If the guest as an USB xHCI
    drivers gives access to USB devices out of the box. See
    {!usb_input_devices}. *)

val usb_input_devices : unit  -> Cmd.t
(** [usb_input_devices ()] are the [usb-kbd] and [usb-tablet]
    USB devices. *)

(** {1:drives Drives} *)

type drive_device =
| Nvme (** On Windows this needs no drivers. *)
| Sata (** On Windows this needs no drivers. *)
| Sata_cdrom (** On Windows this needs no drivers. *)
| Smb (** On Windows this needs no drivers. *)
| Usb
  (** This needs {!device_usb_qemu_xhci}. If the guest has an USB xHCI driver
      this works out of the box. *)
| Usb_cdrom
  (** This needs {!device_usb_qemu_xhci}. If the guest has an USB xHCI driver
      this works out of the box. *)
| Virtio_9p_pci
  (** I don't think that's supported on Windows. *)
| Virtio_blk
  (** On Windows this needs the [viostor] driver. *)
| Virtio_scsi
  (** On Windows this needs the [vioscsi] driver. *)
| Virtio_scsi_cdrom
  (** On Windows this needs the [vioscsi] driver. *)
(** The type for drives devices. *)

val drive_devices : drive_device list
(** [drive_devices] are all the cases of {!drive_device}. *)

val drive_device_to_string : drive_device -> string
(** [drive_device_to_string d] is a string representation of [d]. *)

val drive_device_of_string : string -> (drive_device, string) result
(** [drive_device_of_string s] parses back the result of
    {!drive_device_to_string}. *)

val drive_device_of_path :
  Fpath.t -> (drive_device option * Fpath.t, string) result
(** [drive_of_path p] is a drive from [p] it determines a kind of
    device by parsing a trailing [:DEVICE]. *)

type drive = drive_device * Fpath.t
(** The type for drives a drive device and a path to the disk image. *)

val choose_drive_devices :
  use_virtio:bool -> (drive_device option * Fpath.t) list ->
  (drive list, string) result
(** [choose_drive_devices ~use_virtio] has the logic to default drive
    devices. For now this is done on the list because we have the
    special case for the first drive. This also checks that
    the paths exists and are files or directories depending on the
    {!drive_device} and [realpath] them some of the stuff like [smb] device
    doesn't like relative paths. *)

val drives_cmd : drive list -> Cmd.t
(** [drives_cmd] is the list of drives attached.

    These are specified in boot order. If you need to install on an
    empty disk image, put it first and the ISO afterwards, it will be
    skipped by the bootloader before install and picked up
    afterwards. *)

(** {1:graphics Graphics and serial} *)

val device_virtio_gpu_pci : Cmd.t
(** [device_virtio_gpu_pci] is [-device virtio-gpu-pci].
    On Windows guests this need the [viogpudo] virtio driver. *)

val device_ramfb : Cmd.t
(** [device_ramfb] is [-device ramfb]. A bit unclear when this is useful. *)

val no_graphic : Cmd.t
(** [no_graphic] is [-nographic]. Only run through the console. *)

val display_none : Cmd.t
(** [display_none] is [-display none]. *)

val serial : string option -> string
(** [serial arg] is an argument for [-serial]. If unspecified by
    default this is [mon:stdout]. *)

(** {1:higher Higher-level} *)

val cmd :
  guest_arch:B0_std.Os.Arch.t ->
  accel:string option ->
  mem_size:string option ->
  use_virtio:bool ->
  drives:drive list ->
  graphic:bool ->
  use_ramfb:bool ->
  use_usb_input:bool -> (B0_std.Cmd.t, string) result
