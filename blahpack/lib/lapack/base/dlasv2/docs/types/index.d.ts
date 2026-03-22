

// TypeScript declarations for @stdlib/lapack/base/dlasv2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute SVD of a 2-by-2 triangular matrix
	*/
	(
		f: number,
		g: number,
		h: number,
		ssmin: number,
		ssmax: number,
		snr: number,
		csr: number,
		snl: number,
		csl: number
	): void;
}

/**
* Compute SVD of a 2-by-2 triangular matrix
*/
declare var dlasv2: Routine;

export = dlasv2;
