

// TypeScript declarations for @stdlib/lapack/base/zlaqps

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* QR factorization with column pivoting (blocked panel)
	*/
	(
		M: number,
		N: number,
		offset: number,
		nb: number,
		kb: number,
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
		AUXV: Float64Array,
		strideAUXV: number,
		offsetAUXV: number,
		F: Float64Array,
		strideF1: number,
		strideF2: number,
		offsetF: number
	): Float64Array;
}

/**
* QR factorization with column pivoting (blocked panel)
*/
declare var zlaqps: Routine;

export = zlaqps;
