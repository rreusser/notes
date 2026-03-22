

// TypeScript declarations for @stdlib/lapack/base/dlasq4

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Compute approximate singular value for dqds iteration
	*/
	(
		i0: number,
		n0: number,
		z: Float64Array,
		stride: number,
		offset: number,
		pp: number,
		n0in: number,
		dmin: number,
		dmin1: number,
		dmin2: number,
		dn: number,
		dn1: number,
		dn2: number,
		tau: number,
		ttype: number,
		g: number
	): Float64Array;
}

/**
* Compute approximate singular value for dqds iteration
*/
declare var dlasq4: Routine;

export = dlasq4;
