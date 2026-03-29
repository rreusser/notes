
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var dspr = require( './dspr.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( dspr, 'ndarray', ndarray );


// EXPORTS //

module.exports = dspr;
