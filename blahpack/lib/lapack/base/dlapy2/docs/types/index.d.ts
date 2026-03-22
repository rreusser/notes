

// TypeScript declarations for @stdlib/lapack/base/dlapy2

/**
* Interface describing the ndarray API.
*/
interface Routine {
	/**
	* Return sqrt(x**2 + y**2), taking care not to cause unnecessary overflow.
	*/
	(
		x: number,
		y: number
	): number;
}

/**
* Return sqrt(x**2 + y**2), taking care not to cause unnecessary overflow.
*/
declare var dlapy2: Routine;

export = dlapy2;
