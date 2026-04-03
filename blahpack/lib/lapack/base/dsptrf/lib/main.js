
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsptrf = require( './dsptrf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsptrf, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsptrf;
