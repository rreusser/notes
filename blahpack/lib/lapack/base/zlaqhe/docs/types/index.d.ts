

// TypeScript declarations for @stdlib/lapack/base/zlaqhe

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Equilibrate a Hermitian matrix using scaling factors
	*/
	(
		uplo: string,
		N: number,
		A: Float64Array,
		strideA1: number,
		strideA2: number,
		offsetA: number,
		s: Float64Array,
		strideS: number,
		offsetS: number,
		scond: number,
		amax: number,
		equed: string
	): Float64Array;
}

/**
* Equilibrate a Hermitian matrix using scaling factors
*/
declare var zlaqhe: Routine;

export = zlaqhe;
