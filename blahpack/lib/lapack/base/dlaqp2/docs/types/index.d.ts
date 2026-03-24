

// TypeScript declarations for @stdlib/lapack/base/dlaqp2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Computes a QR factorization with column pivoting using Level 2 BLAS
	*/
	(
		M: number,
		N: number,
		offset: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		JPVT: Int32Array,
		strideJPVT: number,
		offsetJPVT: number,
		TAU: Float64Array,
		strideTAU: number,
		offsetTAU: number,
		VN1: Float64Array,
		strideVN1: number,
		offsetVN1: number,
		VN2: Float64Array,
		strideVN2: number,
		offsetVN2: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Computes a QR factorization with column pivoting using Level 2 BLAS
*/
declare var dlaqp2: Routine;

export = dlaqp2;
