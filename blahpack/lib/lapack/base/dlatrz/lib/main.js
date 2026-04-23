
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dlatrz = require( './dlatrz.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dlatrz, 'ndarray', ndarray );


// EXPORTS //

module.exports = dlatrz;
