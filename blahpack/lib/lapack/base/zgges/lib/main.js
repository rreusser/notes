
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zgges = require( './zgges.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zgges, 'ndarray', ndarray );


// EXPORTS //

module.exports = zgges;
