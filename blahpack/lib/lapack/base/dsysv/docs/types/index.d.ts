

// TypeScript declarations for @stdlib/lapack/base/dsysv

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Solves a real symmetric indefinite system of linear equations using Bunch-Kaufman factorization
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
* Solves a real symmetric indefinite system of linear equations using Bunch-Kaufman factorization
*/
declare var dsysv: Routine;

export = dsysv;
