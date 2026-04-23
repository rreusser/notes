
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dsptrd = require( './dsptrd.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dsptrd, 'ndarray', ndarray );


// EXPORTS //

module.exports = dsptrd;
