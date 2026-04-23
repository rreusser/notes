
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsptri = require( './dsptri.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsptri, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsptri;
