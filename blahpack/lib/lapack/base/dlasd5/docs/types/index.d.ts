

// TypeScript declarations for @stdlib/lapack/base/dlasd5

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute the square root of the i-th eigenvalue of a positive symmetric rank-one modification of a 2-by-2 diagonal matrix.
	*/
	(
		i: number,
		d: Float64Array,
		strideD: number,
		offsetD: number,
		z: Float64Array,
		strideZ: number,
		offsetZ: number,
		delta: Float64Array,
		strideDELTA: number,
		offsetDELTA: number,
		rho: number,
		dsigma: number,
		work: Float64Array,
		strideWORK: number,
		offsetWORK: number
	): Float64Array;
}

/**
* Compute the square root of the i-th eigenvalue of a positive symmetric rank-one modification of a 2-by-2 diagonal matrix.
*/
declare var dlasd5: Routine;

export = dlasd5;
