

// TypeScript declarations for @stdlib/lapack/base/dlasq3

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Check for deflation and compute shift for dqds iteration
	*/
	(
		i0: number,
		n0: number,
		z: Float64Array,
		stride: number,
		offset: number,
		pp: number,
		dmin: number,
		sigma: number,
		desig: number,
		qmax: number,
		nfail: number,
		iter: number,
		ndiv: number,
		ieee: boolean,
		ttype: number,
		dmin1: number,
		dmin2: number,
		dn: number,
		dn1: number,
		dn2: number,
		g: number,
		tau: number
	): Float64Array;
}

/**
* Check for deflation and compute shift for dqds iteration
*/
declare var dlasq3: Routine;

export = dlasq3;
