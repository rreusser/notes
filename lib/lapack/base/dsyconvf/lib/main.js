
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsyconvf = require( './dsyconvf.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsyconvf, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsyconvf;
