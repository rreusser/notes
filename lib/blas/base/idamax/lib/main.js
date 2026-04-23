'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var idamax = require( './idamax.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( idamax, 'ndarray', ndarray );


// EXPORTS //

module.exports = idamax;
