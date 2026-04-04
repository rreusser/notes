

// TypeScript declarations for @stdlib/lapack/base/zlaqhp

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Equilibrates a complex Hermitian matrix in packed storage using scaling factors.
	*/
	(
		uplo: string,
		N: number,
		AP: Float64Array,
		strideAP: number,
		offsetAP: number,
		s: Float64Array,
		strideS: number,
		offsetS: number,
		scond: number,
		amax: number,
		equed: string
	): Float64Array;
}

/**
* Equilibrates a complex Hermitian matrix in packed storage using scaling factors.
*/
declare var zlaqhp: Routine;

export = zlaqhp;
