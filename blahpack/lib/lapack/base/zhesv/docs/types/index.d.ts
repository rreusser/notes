

// TypeScript declarations for @stdlib/lapack/base/zhesv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Complex Hermitian indefinite linear system solver
	*/
	(
		uplo: string,
		N: number,
		nrhs: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		IPIV: Int32Array,
		strideIPIV: number,
		offsetIPIV: number,
		B: Float64Array,
		strideB1: number,
		strideB2: number,
		offsetB: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number
	): Float64Array;
}

/**
* Complex Hermitian indefinite linear system solver
*/
declare var zhesv: Routine;

export = zhesv;
