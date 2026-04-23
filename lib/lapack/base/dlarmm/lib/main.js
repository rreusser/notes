

'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlarmm = require( './dlarmm.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlarmm, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlarmm;
