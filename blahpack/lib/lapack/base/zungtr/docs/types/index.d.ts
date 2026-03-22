

// TypeScript declarations for @stdlib/lapack/base/zungtr

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Generate unitary matrix Q from Hermitian tridiagonal reduction
	*/
	(
		uplo: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		TAU: Float64Array,
		strideTAU: number,
		offsetTAU: number,
		WORK: Float64Array,
		strideWORK: number,
		offsetWORK: number,
		lwork: number
	): Float64Array;
}

/**
* Generate unitary matrix Q from Hermitian tridiagonal reduction
*/
declare var zungtr: Routine;

export = zungtr;
