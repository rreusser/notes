
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsytrfAa = require( './dsytrf_aa.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsytrfAa, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsytrfAa;
