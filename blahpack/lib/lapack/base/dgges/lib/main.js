
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dgges = require( './dgges.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dgges, 'ndarray', ndarray );


// EXPORTS //

module.exports = dgges;
