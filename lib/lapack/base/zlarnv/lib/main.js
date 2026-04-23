
'use strict';

// MODULES //

var setReadOnly = require( '@stdlib/utils/define-nonenumerable-read-only-property' );
var zlarnv = require( './zlarnv.js' );
var ndarray = require( './ndarray.js' );


// MAIN //

setReadOnly( zlarnv, 'ndarray', ndarray );


// EXPORTS //

module.exports = zlarnv;
