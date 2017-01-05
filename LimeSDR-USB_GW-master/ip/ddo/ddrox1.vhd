-- megafunction wizard: %ALTDDIO_OUT%
-- GENERATION: STANDARD
-- VERSION: WM1.0
-- MODULE: ALTDDIO_OUT 

-- ============================================================
-- File Name: ddrox1.vhd
-- Megafunction Name(s):
-- 			ALTDDIO_OUT
--
-- Simulation Library Files(s):
-- 			
-- ============================================================
-- ************************************************************
-- THIS IS A WIZARD-GENERATED FILE. DO NOT EDIT THIS FILE!
--
-- 15.1.2 Build 193 02/01/2016 SJ Lite Edition
-- ************************************************************


--Copyright (C) 1991-2016 Altera Corporation. All rights reserved.
--Your use of Altera Corporation's design tools, logic functions 
--and other software and tools, and its AMPP partner logic 
--functions, and any output files from any of the foregoing 
--(including device programming or simulation files), and any 
--associated documentation or information are expressly subject 
--to the terms and conditions of the Altera Program License 
--Subscription Agreement, the Altera Quartus Prime License Agreement,
--the Altera MegaCore Function License Agreement, or other 
--applicable license agreement, including, without limitation, 
--that your use is for the sole purpose of programming logic 
--devices manufactured by Altera and sold by Altera or its 
--authorized distributors.  Please refer to the applicable 
--agreement for further details.


LIBRARY ieee;
USE ieee.std_logic_1164.all;

LIBRARY altera_mf;
USE altera_mf.altera_mf_components.all;

ENTITY ddrox1 IS
	PORT
	(
		datain_h		: IN STD_LOGIC_VECTOR (0 DOWNTO 0);
		datain_l		: IN STD_LOGIC_VECTOR (0 DOWNTO 0);
		outclock		: IN STD_LOGIC ;
		dataout		: OUT STD_LOGIC_VECTOR (0 DOWNTO 0)
	);
END ddrox1;


ARCHITECTURE SYN OF ddrox1 IS

	SIGNAL sub_wire0	: STD_LOGIC_VECTOR (0 DOWNTO 0);

BEGIN
	dataout    <= sub_wire0(0 DOWNTO 0);

	ALTDDIO_OUT_component : ALTDDIO_OUT
	GENERIC MAP (
		extend_oe_disable => "OFF",
		intended_device_family => "Cyclone IV E",
		invert_output => "OFF",
		lpm_hint => "UNUSED",
		lpm_type => "altddio_out",
		oe_reg => "UNREGISTERED",
		power_up_high => "OFF",
		width => 1
	)
	PORT MAP (
		datain_h => datain_h,
		datain_l => datain_l,
		outclock => outclock,
		dataout => sub_wire0
	);



END SYN;

-- ============================================================
-- CNX file retrieval info
-- ============================================================
-- Retrieval info: LIBRARY: altera_mf altera_mf.altera_mf_components.all
-- Retrieval info: PRIVATE: INTENDED_DEVICE_FAMILY STRING "Cyclone IV E"
-- Retrieval info: CONSTANT: EXTEND_OE_DISABLE STRING "OFF"
-- Retrieval info: CONSTANT: INTENDED_DEVICE_FAMILY STRING "Cyclone IV E"
-- Retrieval info: CONSTANT: INVERT_OUTPUT STRING "OFF"
-- Retrieval info: CONSTANT: LPM_HINT STRING "UNUSED"
-- Retrieval info: CONSTANT: LPM_TYPE STRING "altddio_out"
-- Retrieval info: CONSTANT: OE_REG STRING "UNREGISTERED"
-- Retrieval info: CONSTANT: POWER_UP_HIGH STRING "OFF"
-- Retrieval info: CONSTANT: WIDTH NUMERIC "1"
-- Retrieval info: USED_PORT: datain_h 0 0 1 0 INPUT NODEFVAL "datain_h[0..0]"
-- Retrieval info: CONNECT: @datain_h 0 0 1 0 datain_h 0 0 1 0
-- Retrieval info: USED_PORT: datain_l 0 0 1 0 INPUT NODEFVAL "datain_l[0..0]"
-- Retrieval info: CONNECT: @datain_l 0 0 1 0 datain_l 0 0 1 0
-- Retrieval info: USED_PORT: dataout 0 0 1 0 OUTPUT NODEFVAL "dataout[0..0]"
-- Retrieval info: CONNECT: dataout 0 0 1 0 @dataout 0 0 1 0
-- Retrieval info: USED_PORT: outclock 0 0 0 0 INPUT_CLK_EXT NODEFVAL "outclock"
-- Retrieval info: CONNECT: @outclock 0 0 0 0 outclock 0 0 0 0
-- Retrieval info: GEN_FILE: TYPE_NORMAL ddrox1.vhd TRUE FALSE
-- Retrieval info: GEN_FILE: TYPE_NORMAL ddrox1.qip TRUE FALSE
-- Retrieval info: GEN_FILE: TYPE_NORMAL ddrox1.bsf TRUE TRUE
-- Retrieval info: GEN_FILE: TYPE_NORMAL ddrox1_inst.vhd TRUE TRUE
-- Retrieval info: GEN_FILE: TYPE_NORMAL ddrox1.inc TRUE TRUE
-- Retrieval info: GEN_FILE: TYPE_NORMAL ddrox1.cmp TRUE TRUE
-- Retrieval info: GEN_FILE: TYPE_NORMAL ddrox1.ppf TRUE FALSE
