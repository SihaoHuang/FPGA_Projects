// Serial_flash_loader.v

// Generated using ACDS version 15.0 145

`timescale 1 ps / 1 ps
module Serial_flash_loader (
		input  wire  noe_in  // noe_in.noe
	);

	altera_serial_flash_loader #(
		.INTENDED_DEVICE_FAMILY  ("Cyclone IV E"),
		.ENHANCED_MODE           (1),
		.ENABLE_SHARED_ACCESS    ("OFF"),
		.ENABLE_QUAD_SPI_SUPPORT (0),
		.NCSO_WIDTH              (1)
	) serial_flash_loader_0 (
		.noe_in (noe_in)  // noe_in.noe
	);

endmodule
