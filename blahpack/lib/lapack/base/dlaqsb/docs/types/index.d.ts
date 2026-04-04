

// TypeScript declarations for @stdlib/lapack/base/dlaqsb

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Equilibrates a real symmetric band matrix using scaling factors.
	*/
	(
		uplo: string,
		N: number,
		kd: number,
		AB: Float64Array,
		strideAB1: number,
		strideAB2: number,
		offsetAB: number,
		s: Float64Array,
		strideS: number,
		offsetS: number,
		scond: number,
		amax: number,
		equed: string
	): Float64Array;
}

/**
* Equilibrates a real symmetric band matrix using scaling factors.
*/
declare var dlaqsb: Routine;

export = dlaqsb;
